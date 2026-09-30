;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The external agent: the lab's tools handed to an agent that runs
;; somewhere else, in place of the loop of agent.lisp.  A developer's
;; own agent (Claude Code run headless is the one that ships, in
;; external/claude-code.mjs) works on a session exactly as the lab's
;; would -- the same system prompt, the same tools run the same way on
;; the same model file, the same log for the page to follow -- while
;; the calls to the language model are that agent's own business and
;; never pass through this code or its gate.
;;
;; Two doors, both shut (404) unless *external-agent?* is true:
;;
;;   <prefix>/api/agent   the session's side of a prompt: opened, told,
;;                        finished.  What the prompt door and run-prompt
;;                        do between them for the lab's own loop.
;;   <prefix>/mcp         the tools, as a Model Context Protocol server
;;                        (the Streamable HTTP transport, stateless:
;;                        each POST is one JSON-RPC message and its
;;                        answer is plain JSON, no stream is offered).
;;
;; Both name the session (session=) and carry its owner key, like every
;; other door that changes one.  Neither applies the caps or the human
;; check of the public doors: whoever turns this on has decided who may
;; reach it.
;;

(defvar *external-lock* (bt:make-lock "prompt-lab external agent")
  "Held around each tool an external agent runs: the lab's own loop runs
a turn's tools one after another, and an agent that asks for several at
once gets the same.")

(defun external-off (req ent)
  (refuse req ent net.aserve:*response-not-found* "This lab takes no external agent."))

(defun tool-name (tool)
  (cdr (assoc "name" tool :test #'string=)))


;;
;; The agent door.
;;

(defun agent-brief (session)
  "What an external agent needs to work on SESSION as the lab's own would."
  (h "session" (session-id session)
     "system" (system-text)
     "model" *model*
     "effort" *effort*
     "max_rounds" *max-rounds*
     "mcp" (format nil "~a/mcp?session=~a" *url-prefix* (session-id session))
     "tools" (map 'list #'tool-name (tool-definitions))))

(defun agent-door (req ent)
  "POST <prefix>/api/agent {event, session?, text, usage?}: an external
agent's side of one prompt.  The events:
  prompt   TEXT is the visitor's prompt.  Without a session one is opened,
           and its owner key answered, once.  The session is claimed as
           the prompt door claims it; the answer is the brief: the system
           prompt, the model and effort the lab itself would use, the
           tools' names and the address of the MCP door.
  text     TEXT is the agent's progress, for the log.
  done     TEXT is the agent's reply; USAGE, when given, is its token
           counts in the Messages API's names.  The session is released.
  stopped  TEXT says why the agent gave up.  The session is released."
  (let* ((json (request-json req))
         (event (and json (gethash "event" json)))
         (text (and json (gethash "text" json)))
         (named (or (and json (gethash "session" json)) (query-value req "session")))
         (session (requested-session req json)))
    (flet ((said () (and (stringp text) (string-trim '(#\space #\tab #\newline #\return) text)))
           (release ()
             (setf (session-busy? session) nil)
             (save-session! session)
             (respond-json req ent (h "ok" t))))
      (cond ((not *external-agent?*) (external-off req ent))
            ((and named (null session)) (no-such-session req ent))
            ((and session (not (owner-request? req session json))) (not-yours req ent))
            ((equal event "prompt")
             (cond ((not (plusp (length (said)))) (refuse req ent "Say what to build."))
                   ((> (length text) *max-prompt-length*)
                    (refuse req ent "A prompt may have ~a characters at most." *max-prompt-length*))
                   (t (let* ((opened? (null session))
                             (session (or session (make-session :address (client-address req)))))
                        (cond ((not (claim! session))
                               (refuse req ent "Still working on the previous request."))
                              (t (when opened?
                                   (log-event session :note "Session ~a opened for an external agent."
                                              (session-id session)))
                                 (log-event session :prompt "~a" (said))
                                 (setf (session-messages session)
                                       (append (session-messages session)
                                               (list (h "role" "user" "content" (said)))))
                                 (save-session! session)
                                 (let ((brief (agent-brief session)))
                                   ;; the key goes to whoever opened the session, once
                                   (when opened? (setf (gethash "owner" brief) (session-owner session)))
                                   (respond-json req ent brief))))))))
            ((null session) (refuse req ent "Name the session."))
            ((equal event "text")
             (when (plusp (length (said))) (log-event session :text "~a" (said)))
             (respond-json req ent (h "ok" t)))
            ((equal event "done")
             (let ((reply (if (plusp (length (said))) (said) "(The agent finished without a reply.)"))
                   (usage (gethash "usage" json)))
               (log-event session :done "~a" reply)
               ;; the reply joins the conversation, so the lab's own loop
               ;; can take the session up where this agent left it
               (setf (session-messages session)
                     (append (session-messages session)
                             (list (h "role" "assistant"
                                      "content" (list (h "type" "text" "text" reply))))))
               (when (hash-table-p usage) (add-usage session usage))
               (release)))
            ((equal event "stopped")
             (log-event session :stopped "~a" (if (plusp (length (said)))
                                                  (said)
                                                  "The external agent stopped."))
             (release))
            (t (refuse req ent "Unknown event; one of prompt, text, done, stopped."))))))


;;
;; The MCP door.
;;

(defparameter *mcp-protocol-versions* '("2025-06-18" "2025-11-25" "2025-03-26")
  "The protocol revisions this door speaks, the one it offers first.  A
client that asks for another of them gets the one it asked for.")

(defun rpc-result (id result)
  (h "jsonrpc" "2.0" "id" id "result" result))

(defun rpc-error (id code control &rest args)
  (h "jsonrpc" "2.0" "id" id
     "error" (h "code" code "message" (apply #'format nil control args))))

(defun mcp-tool (tool)
  "One of the lab's tool definitions, the Messages API's shape, in MCP's."
  (let* ((table (->json tool))
         (schema (gethash "input_schema" table)))
    ;; no required input: the key is left out (an empty list would
    ;; encode as null, which is no array)
    (unless (gethash "required" schema) (remhash "required" schema))
    (h "name" (gethash "name" table)
       "description" (gethash "description" table)
       "inputSchema" schema)))

(defun mcp-content (blocks)
  "A tool's content blocks (tools.lisp: text and image alists in the
Messages API's shape) as MCP content."
  (flet ((field (name alist) (cdr (assoc name alist :test #'string=))))
    (mapcar #'(lambda (block)
                (if (equal (field "type" block) "image")
                    (let ((source (field "source" block)))
                      (h "type" "image"
                         "data" (field "data" source)
                         "mimeType" (field "media_type" source)))
                    (h "type" "text" "text" (or (field "text" block) ""))))
            blocks)))

(defun mcp-call (session name arguments)
  "Run tool NAME on SESSION for an external agent, logged as the lab's own
loop logs it.  Returns the MCP result."
  (log-event session :tool "~a" name)
  (multiple-value-bind (blocks error?)
      (bt:with-lock-held (*external-lock*)
        (handler-case (run-tool session name (and (hash-table-p arguments)
                                                  (alexandria:hash-table-alist arguments)))
          (error (condition)
            (values (list (text-result "~a" condition)) t))))
    (when error?
      (log-event session :tool-error "~a: ~a" name
                 (or (cdr (assoc "text" (first blocks) :test #'string=)) "failed")))
    (h "content" (mcp-content blocks)
       "isError" (if error? t 'yason:false))))

(defun mcp-answer (session message)
  "The JSON-RPC answer to request MESSAGE, a hash table."
  (let ((id (gethash "id" message))
        (method (gethash "method" message))
        (params (gethash "params" message)))
    (flet ((param (name) (and (hash-table-p params) (gethash name params))))
      (cond ((equal method "initialize")
             (rpc-result id (h "protocolVersion"
                               (or (find (param "protocolVersion") *mcp-protocol-versions* :test #'equal)
                                   (first *mcp-protocol-versions*))
                               "capabilities" (h "tools" (h))
                               "serverInfo" (h "name" "prompt-lab" "version" "1")
                               "instructions" "The tools of one prompt-lab session: its model file, its package, its checks.")))
            ((equal method "ping") (rpc-result id (h)))
            ((equal method "tools/list")
             (rpc-result id (h "tools" (map 'list #'mcp-tool (tool-definitions)))))
            ((equal method "tools/call")
             (let ((name (param "name")))
               (if (find name (tool-definitions) :key #'tool-name :test #'equal)
                   (rpc-result id (mcp-call session name (param "arguments")))
                   (rpc-error id -32602 "Unknown tool: ~a" name))))
            (t (rpc-error id -32601 "Method not found: ~a" method))))))

(defun mcp-door (req ent)
  "<prefix>/mcp?session=<id>: the session's tools as an MCP server.  POST
one JSON-RPC message; a request is answered in JSON, a notification with
202 and nothing.  There is no stream to GET and no state to DELETE."
  (let ((session (requested-session req)))
    (cond ((not *external-agent?*) (external-off req ent))
          ((null session) (no-such-session req ent))
          ((not (owner-request? req session)) (not-yours req ent))
          ((not (eq (net.aserve:request-method req) :post))
           (refuse req ent net.aserve:*response-method-not-allowed*
                   "POST one JSON-RPC message here; no stream is offered."))
          (t (let ((message (request-json req)))
               (cond ((null message)
                      (respond-json req ent (rpc-error nil -32700 "One JSON-RPC message, an object, is expected.")
                                    net.aserve:*response-bad-request*))
                     ;; a notification, or an answer to something never
                     ;; asked: taken, and nothing said
                     ((or (not (nth-value 1 (gethash "id" message)))
                          (not (stringp (gethash "method" message))))
                      (net.aserve:with-http-response (req ent :response net.aserve:*response-accepted*)
                        (net.aserve:with-http-body (req ent))))
                     (t (respond-json req ent (mcp-answer (touch session) message)))))))))
