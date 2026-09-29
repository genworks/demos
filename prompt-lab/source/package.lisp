;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :gdl-user)

(gwl:define-package :prompt-lab
    (:export #:make-session
             #:find-session
             #:delete-session
             #:session-id
             #:session-package-name
             #:session-model-file
             #:tool-definitions
             #:run-tool
             #:primer-text
             #:run-prompt
             #:start-prompt!
             #:viewer
             #:publish-prompt-lab!
             #:*workspace-root*
             #:*url-prefix*
             #:*console-base*
             #:*max-prompts-per-session*
             #:*render-tool?*
             #:*max-sessions-per-address*
             #:*max-prompts-per-address*
             #:*turnstile-site-key*
             #:*turnstile-verify-url*
             #:*turnstile-secret-file*
             #:*browsing?*))
