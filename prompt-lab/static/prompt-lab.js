/*
 * Copyright (c) 2026 Genworks International
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 *
 * The prompt lab's page: it opens or resumes a session, posts prompts,
 * polls the session's state and reloads the viewer after every build.
 * It also keeps the page's two layouts (prompt-lab.css) in step with
 * the viewer in its frame, and wears the skin the visitor chose.
 *
 * Kept in ASCII: characters beyond it are written as escapes, so the
 * file reads the same whatever a server says its encoding is.
 */
(function () {
  var lab = window.promptLab || { house: 'workstation', skins: [], aliases: {}, skin: null, pinned: null };
  var base = location.pathname.replace(/\/+$/, '');
  // the lab's doors are under its prefix, which the page may not be at
  // (the classic page at <prefix>/classic once the sheet took the prefix)
  var doors = lab.prefix || base;
  var params = new URLSearchParams(location.search);
  var session = params.get('session');
  // ?archive=<id>: an archived session, read-only; ?browse=live|archive:
  // the listings.  Anything else is the lab itself.
  var archiveId = params.get('archive'), browse = params.get('browse');
  var $ = function (id) { return document.getElementById(id); };

  // An address of this page; a skin pinned on the address stays on it.
  function pageUrl(query) {
    var q = query || '';
    if (lab.pinned) q += (q ? '&' : '') + 'skin=' + encodeURIComponent(lab.pinned);
    return base + (q ? '?' + q : '');
  }

  // The owner keys of the sessions this browser opened (id -> key): the
  // session door hands one over, and only a request carrying it may
  // prompt, edit or pay; without it a session is watched, read-only.
  var owners = {};
  try { owners = JSON.parse(localStorage.getItem('prompt-lab-owners') || '{}') || {}; } catch (e) {}
  function ownerKey() { return session ? owners[session] || null : null; }
  function keepOwner(id, key) {
    owners[id] = key;
    try { localStorage.setItem('prompt-lab-owners', JSON.stringify(owners)); } catch (e) {}
  }
  var editable = true;
  // The wallet: an id the gate mints at the first top-up, kept in this
  // browser; it names the visitor's credit on every build.
  var wallet = null;
  try { wallet = localStorage.getItem('prompt-lab-wallet'); } catch (e) {}
  if (params.get('wallet')) { wallet = params.get('wallet'); try { localStorage.setItem('prompt-lab-wallet', wallet); } catch (e) {} }
  var checkout = params.get('checkout'), topupCancelled = params.get('topup') === 'cancelled';
  var topupButtonsMade = false;


  //
  // The lab as an app.  Installed (the manifest, and the worker that
  // keeps the page's shell), it opens from its icon at ?app=1 and takes
  // up the session this browser was last in.  The page is all that is
  // kept on the device: a model is built, drawn and saved by the lab's
  // engine, so when the lab cannot be reached the page says so, shows
  // that session as it last saw it, and waits.
  //

  var launched = params.get('app') === '1', resumed = false;
  var unreachable = false, configLoaded = false, shownLast = false;

  function lastSeen() {
    try { return JSON.parse(localStorage.getItem('prompt-lab-last') || 'null'); } catch (e) { return null; }
  }

  function keepLast(state) {
    try {
      localStorage.setItem('prompt-lab-last', JSON.stringify({
        session: state.session, created: state.created, log: state.log || [],
        usage: state.usage, meter: state.meter,
        model_source: state.model_source, model_defined: !!state.model_defined
      }));
    } catch (e) {}
  }

  function forgetLast() { try { localStorage.removeItem('prompt-lab-last'); } catch (e) {} }

  if (launched && !session && !archiveId && !browse) {
    var last = lastSeen();
    if (last && last.session && owners[last.session]) {
      session = last.session;
      resumed = true;
      history.replaceState(null, '', pageUrl('session=' + session));
    }
  }

  //
  // The model file's box.  A plain textarea as the page arrives; the
  // structured editor (static/editor.js, built from ../editor) is mounted
  // over it once its script has loaded: colours, folding by s-expression,
  // matched parentheses.  Everything else here speaks to `source` and
  // does not care which it is.
  //

  var editor = null;
  var source = {
    get: function () { return editor ? editor.value : $('source').value; },
    set: function (text) { if (editor) editor.value = text; else $('source').value = text; },
    lock: function (flag) { $('source').readOnly = !!flag; if (editor) editor.setReadOnly(!!flag); },
    hint: function (text) { $('source').placeholder = text; if (editor) editor.setPlaceholder(text); }
  };

  function edited() {
    if (!sourceDirty) editBase = lastSource;
    sourceDirty = true;
    $('save').disabled = false;
    $('source-state').textContent = 'edited';
  }

  function mountEditor() {
    if (editor || !window.PromptLabEditor) return;
    try { editor = window.PromptLabEditor.mount($('source'), { onEdit: edited }); } catch (e) { editor = null; return; }
    lab.editor = editor;        // within reach of a console, and of a test
    document.body.classList.add('has-editor');
    $('fold-row').hidden = false;
  }

  // The lab answers, or it does not.
  function reach(there) {
    if (unreachable === !there) return;
    unreachable = !there;
    document.body.classList.toggle('unreachable', unreachable);
    $('offline-banner').hidden = !unreachable;
    source.lock(unreachable || !editable);
    if (unreachable) {
      status('No Connection');
      $('build').disabled = true;
      $('save').disabled = true;
      $('busy').hidden = true;
      $('quota').textContent = '';
    } else {
      // what the page asks once, it may not have been able to ask yet
      if (!configLoaded) loadConfig();
      if (!session) {
        status('User Input');
        $('build').disabled = !turnstileReady() || potEmpty();
      }
    }
  }

  // The session as this browser last saw it, shown once, when the lab
  // cannot be asked.
  function showLast() {
    var last = lastSeen();
    if (shownLast || rendered || !last || last.session !== session) return;
    shownLast = true;
    renderLog(last.log || []);
    $('usage').textContent = usageText(last.usage, last.meter, true);
    $('status-session').textContent = 'Session ' + last.session;
    source.set(last.model_source || '');
    lastSource = last.model_source;
    if (last.model_defined) $('viewer-empty').textContent = 'The model is drawn by the lab\'s engine. It will be here when the lab answers.';
  }

  // A session taken up at launch that the lab no longer has: a new
  // start, without a word about it.
  function startAfresh() {
    resumed = false;
    forgetLast();
    session = null;
    logCount = 0;
    $('log').innerHTML = '';
    $('usage').textContent = '';
    $('status-session').textContent = '';
    $('welcome').hidden = false;
    source.set('');
    lastSource = null;
    history.replaceState(null, '', pageUrl(''));
  }


  //
  // Two layouts, one document.  On a desk the panes tile a frame; on a
  // phone they are screens behind the tabs, and the viewer's frame shows
  // the model with one panel under it.  The stylesheet decides by the
  // same question asked here.
  //

  var phoneQuery = window.matchMedia('(max-width: 760px), (pointer: coarse) and (max-height: 520px)');
  function phone() { return phoneQuery.matches; }
  var screen = 'prompt', menusShown = false;

  function frameDocument() {
    try { return $('viewer').contentDocument || null; } catch (e) { return null; }
  }

  // The stylesheets of the skin in the viewer's frame: the one that
  // says the sluice in the tokens (the sluice's own, or the lab's copy
  // where the sluice is older than its skins), the phone's, and the
  // skin's.  Without the first the frame holds something else (an
  // error's page), and is left alone.
  function frameSheets(doc) {
    var found = { viewer: null, phone: null, skins: [] };
    if (!doc || !doc.head || !doc.body) return found;
    var links = doc.querySelectorAll('link[rel="stylesheet"]');
    for (var i = 0; i < links.length; i++) {
      var href = links[i].getAttribute('href') || '';
      if (/\/sluice-static\/skinned\.css/.test(href) || /\/static\/prompt-lab-viewer\.css/.test(href)) found.viewer = links[i];
      else if (/\/static\/prompt-lab-phone\.css/.test(href)) found.phone = links[i];
      else if (/\/sluice-static\/skin-/.test(href)) found.skins.push(links[i]);
      else if (/\/static\/prompt-lab-/.test(href)) found.skins.push(links[i]);
    }
    return found;
  }

  // The viewer's own layout follows the page's.  A frame opened for a
  // phone has the phone's sheet already (mode=phone); one opened on a
  // desk is given it here, the day the page becomes an app.  After
  // that it is classes on the frame's body, which that sheet reads.
  function syncFrame() {
    var doc = frameDocument(), sheets = frameSheets(doc);
    if (!sheets.viewer) return;
    if (phone() && !sheets.phone && lab.phone_css) {
      var link = doc.createElement('link');
      link.rel = 'stylesheet'; link.href = lab.phone_css;
      sheets.viewer.parentNode.insertBefore(link, sheets.viewer.nextSibling);
    }
    doc.body.classList.toggle('pl-desk', !phone());
    doc.body.classList.toggle('pl-panel-parts', phone() && screen === 'parts');
    doc.body.classList.toggle('pl-menus', phone() && menusShown);
  }

  function show(name) {
    screen = name;
    document.body.setAttribute('data-screen', name);
    var buttons = $('tabs').querySelectorAll('button');
    for (var i = 0; i < buttons.length; i++)
      buttons[i].setAttribute('aria-pressed', buttons[i].getAttribute('data-screen') === name ? 'true' : 'false');
    syncFrame();
    if (name === 'prompt') { var box = $('listener'); box.scrollTop = box.scrollHeight; }
  }

  function sheet(open) {
    document.body.classList.toggle('sheet-open', open);
    $('scrim').hidden = !open;
    $('more').setAttribute('aria-expanded', open ? 'true' : 'false');
  }

  // The address of the viewer for its frame: in the page's skin, and
  // laid out for a phone when the page is.
  function dressed(url, framed) {
    if (lab.skin) url += '&skin=' + encodeURIComponent(lab.skin.name);
    if (framed && phone()) url += '&mode=phone';
    return url;
  }


  //
  // Skins (SKIN-API.md): one stylesheet over the base sheet, here and in
  // the viewer's frame.  The choice is this browser's.
  //

  function findSkin(name) {
    if (lab.aliases && lab.aliases[name]) name = lab.aliases[name];
    for (var i = 0; i < lab.skins.length; i++) if (lab.skins[i].name === name) return lab.skins[i];
    return null;
  }

  // The colour a phone's browser paints around the page: the skin's
  // label strip, which is what the page's top edge is made of.
  function themeColor() {
    var value = getComputedStyle(document.documentElement).getPropertyValue('--pl-label-bg').trim();
    if (value) $('theme-color').setAttribute('content', value);
  }

  // The frame wears what the page wears.  It was opened in the page's
  // skin (skin= on its address), so there is something to do only when
  // the skin has changed since.
  function dressFrame() {
    var doc = frameDocument(), sheets = frameSheets(doc);
    if (!sheets.viewer) return;
    var want = lab.skin ? lab.skin.href : null;
    var have = sheets.skins.length === 1 ? sheets.skins[0].getAttribute('href') : null;
    if (sheets.skins.length < 2 && have === want) return;
    sheets.skins.forEach(function (link) { link.parentNode.removeChild(link); });
    if (want) {
      var link = doc.createElement('link');
      link.rel = 'stylesheet'; link.href = want;
      doc.head.appendChild(link);
    }
  }

  function setSkin(name) {
    lab.skin = findSkin(name);
    try { localStorage.setItem('prompt-lab-skin', lab.skin ? lab.skin.name : lab.house); } catch (e) {}
    // the menu outranks a skin pinned on the address
    if (lab.pinned) {
      lab.pinned = null;
      var p = new URLSearchParams(location.search);
      p.delete('skin');
      var q = p.toString();
      history.replaceState(null, '', base + (q ? '?' + q : ''));
      markLinks();
    }
    document.documentElement.setAttribute('data-skin', lab.skin ? lab.skin.name : lab.house);
    var link = $('skin-css');
    if (lab.skin) {
      if (!link) {
        link = document.createElement('link');
        link.id = 'skin-css'; link.rel = 'stylesheet';
        document.head.appendChild(link);
      }
      link.onload = themeColor;
      link.href = lab.skin.href;
    } else if (link) {
      link.parentNode.removeChild(link);
    }
    themeColor();
    dressFrame();
    if (viewerUrl) $('viewer-tab').href = viewerTabUrl();
  }

  function makeSkinMenu() {
    if (!lab.skins.length) return;
    var select = $('skin');
    function option(name, label) {
      var o = document.createElement('option');
      o.value = name; o.textContent = label;
      select.appendChild(o);
    }
    option(lab.house, lab.house.charAt(0).toUpperCase() + lab.house.slice(1));
    lab.skins.forEach(function (s) { option(s.name, s.label); });
    select.value = lab.skin ? lab.skin.name : lab.house;
    select.addEventListener('change', function () { setSkin(select.value); sheet(false); });
    $('skin-row').hidden = false;
  }

  // The listings' links keep a pinned skin.
  function markLinks() {
    $('browse-live').href = pageUrl('browse=live');
    $('browse-archive').href = pageUrl('browse=archive');
  }


  //
  // The documentation line at the foot of the frame: what the thing
  // under the pointer is, or what a click on it does.  The viewer's
  // frame reports through the same line.
  //

  var idleDoc = 'Describe a part; a modeling agent builds it as a parametric Gendl model you can inspect and edit.';

  function documentation(target) {
    if (!target || !target.closest) return '';
    var el = target.closest('[data-doc], [title]');
    if (!el) return '';
    var text = el.getAttribute('data-doc') || el.getAttribute('title') || '';
    if (!text) return '';
    var acts = el.closest('a, button, select, input, textarea, label, summary, [onclick], [role="button"]');
    return (acts ? 'Mouse-L: ' : '') + text;
  }

  function showDocumentation(text) { $('mouse-doc').textContent = text || idleDoc; }

  function watchPointer(doc) {
    doc.addEventListener('mouseover', function (event) { showDocumentation(documentation(event.target)); });
    doc.documentElement.addEventListener('mouseleave', function () { showDocumentation(''); });
  }

  function tick() {
    $('status-time').textContent = new Date().toLocaleString([], { weekday: 'short', day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' });
  }

  function status(state) {
    var el = $('status-state');
    el.textContent = state;
    el.className = state === 'Run' ? 'busy' : '';
  }


  // Money on this page is MODELING CREDITS, free and bought alike; the
  // only dollars are on the buy buttons.  A credit is a cent of balance.
  function credits(n) { return (n == null ? 0 : Math.round(n)).toLocaleString(); }
  function dollars(c) { return '$' + (c / 100).toFixed(c % 100 ? 2 : 0); }
  // what the I-th price on sale buys: the gate's topup_credits, beside
  // its topup_amounts; a cent a credit from a gate that sends none
  function creditsSold(offer, amount, i) {
    var sold = offer.topup_credits && offer.topup_credits[i];
    return sold > 0 ? sold : amount;
  }
  // the prices on sale, in cents.  A gate older than its configuration
  // answers each offer as the (cents credits) pair it was given, and
  // refuses every amount at checkout: nothing is offered then, rather
  // than buttons that fail (they read $NaN before, 2026-10-01).
  function topupAmounts(offer) {
    var amounts = offer.topup_amounts || [];
    if (amounts.some(Array.isArray)) return [];
    return amounts.filter(function (a) { return a > 0; });
  }
  var publishableKey = '', embeddedCheckout = null, chosenAmount = null;

  // what is left, in large figures above the meter; low is a tenth of
  // the most there can be, or less
  function showCreditsLeft(left, unit, most) {
    $('credits-left').textContent = credits(Math.max(0, left));
    $('credits-left-unit').textContent = unit;
    var figure = $('credits-figure');
    figure.hidden = false;
    figure.classList.toggle('low', left > 0 && most > 0 && left <= most / 10);
    figure.classList.toggle('out', !(left > 0));
  }

  function renderSpend(spend) {
    if (!spend) return;
    document.body.classList.add('has-spend');
    var used = spend.credits_used || 0, free = spend.credits_free || 0;
    var text = 'this session: ' + credits(used) + ' of ' + credits(free) + ' free credits';
    if (spend.credits_from_wallet > 0) text += ' \u00b7 ' + credits(spend.credits_from_wallet) + ' from your balance';
    if (spend.credits_balance != null) text += ' \u00b7 balance: ' + credits(spend.credits_balance) + ' credits';
    $('spend').textContent = text;
    showCreditsLeft(Math.max(0, free - used) + (spend.credits_balance || 0),
                    'modeling credits left', free + (spend.credits_balance || 0));
    var meter = $('spend-meter'), most = free || 100;
    meter.setAttribute('aria-valuemax', most);
    meter.setAttribute('aria-valuenow', Math.min(used, most));
    meter.firstElementChild.style.width = (100 * Math.min(used, most) / most) + '%';
    $('status-credits').textContent = credits(used) + ' of ' + credits(free) + ' free credits'
      + (spend.credits_balance ? ' \u00b7 balance ' + credits(spend.credits_balance) : '');
    if (spend.publishable_key) publishableKey = spend.publishable_key;
    if (spend.topup && !topupButtonsMade && topupAmounts(spend).length) {
      topupButtonsMade = true;
      topupAmounts(spend).forEach(function (amount, i) {
        var sold = creditsSold(spend, amount, i);
        var b = document.createElement('button');
        b.type = 'button';
        b.textContent = credits(sold) + ' credits for ' + dollars(amount);
        b.setAttribute('data-doc', 'Buy ' + credits(sold) + ' modeling credits by card.');
        b.addEventListener('click', function () { startTopup(amount); });
        $('topup-buttons').appendChild(b);
      });
      $('topup').hidden = false;
    }
  }

  //
  // The community pot.  A lab whose gate keeps one has no free credits a
  // session and no balance a visitor: ONE pot of modeling credits that
  // every build, anyone's, draws on and anyone may add to, up to its cap.
  // At zero nothing builds, and the page asks for a top-up.  The lab
  // names it in its configuration and with every state (pot: what it
  // holds, its cap, the room left, the amounts on sale); the owner's
  // spend says what this session has drawn and this browser has put in.
  //

  var pot = null, potButtons = [];
  var DOT = ' ' + String.fromCharCode(183) + ' ';

  function potEmpty() { return !!pot && !(pot.credits > 0); }

  function renderPot(p, spend) {
    if (!p) return;
    pot = p;
    var empty = potEmpty();
    document.body.classList.add('has-spend');
    document.body.classList.add('has-pot');
    document.body.classList.toggle('pot-empty', empty);
    showCreditsLeft(p.credits, 'modeling credits left in the community pot', p.max);
    var text = 'it holds at most ' + credits(p.max);
    if (spend && spend.credits_used > 0) text += DOT + 'this session has drawn ' + credits(spend.credits_used);
    if (spend && spend.contributed > 0) text += DOT + 'you have added ' + credits(spend.contributed);
    $('spend').textContent = text;
    var meter = $('spend-meter'), most = p.max || 100, held = Math.max(0, Math.min(p.credits, most));
    meter.setAttribute('aria-label', 'Modeling credits in the pot');
    meter.setAttribute('aria-valuemax', most);
    meter.setAttribute('aria-valuenow', held);
    meter.firstElementChild.style.width = (100 * held / most) + '%';
    $('pot-note').hidden = false;
    $('about-pot').hidden = false;
    $('pot-empty').hidden = !empty;
    $('status-credits').textContent = empty ? 'the pot is empty' : 'pot: ' + credits(p.credits) + ' credits';
    if (p.publishable_key) publishableKey = p.publishable_key;
    var amounts = topupAmounts(p);
    if (p.topup && !topupButtonsMade && amounts.length) {
      topupButtonsMade = true;
      $('topup-label').textContent = 'Add to the pot:';
      amounts.forEach(function (amount, i) {
        var sold = creditsSold(p, amount, i);
        var b = document.createElement('button');
        b.type = 'button';
        b.textContent = credits(sold) + ' credits for ' + dollars(amount);
        b.addEventListener('click', function () { startTopup(amount); });
        $('topup-buttons').appendChild(b);
        potButtons.push({ amount: amount, credits: sold, button: b });
      });
      $('topup').hidden = false;
    }
    // a pot sells no more than it has room for, in credits
    var fits = 0;
    potButtons.forEach(function (entry) {
      var fit = entry.credits <= p.room;
      if (fit) fits++;
      entry.button.disabled = !fit;
      entry.button.setAttribute('data-doc', fit
        ? 'Add ' + credits(entry.credits) + ' modeling credits to the pot, by card. Everyone builds on them.'
        : 'The pot has no room for ' + credits(entry.credits) + ' more credits.');
    });
    var full = potButtons.length > 0 && fits === 0;
    $('pot-full').hidden = !full;
    if (full) {
      $('pot-full-text').textContent = 'The pot is as full as it gets: it holds at most ' + credits(p.max) + ' credits.'
        + (p.own_lab_url ? ' To build without sharing one,' : '');
      $('own-lab').hidden = !p.own_lab_url;
      if (p.own_lab_url) { $('own-lab').href = p.own_lab_url; $('own-lab').textContent = (p.own_lab_label || 'run a lab of your own') + '.'; }
    }
    if (empty) {
      $('build').disabled = true;
      if (!$('busy') || $('busy').hidden) $('quota').textContent = 'the community pot is empty';
    }
  }

  function note(text) { var el = $('spend-note'); el.textContent = text || ''; el.hidden = !text; }

  function keepWallet(r) {
    if (r && r.wallet) { wallet = r.wallet; try { localStorage.setItem('prompt-lab-wallet', wallet); } catch (e) {} }
  }

  function loadStripeJs() {
    if (window.Stripe) return Promise.resolve();
    return new Promise(function (resolve, reject) {
      var s = document.createElement('script');
      s.src = 'https://js.stripe.com/v3/'; s.async = true;
      s.onload = resolve; s.onerror = function () { reject(new Error('Stripe.js did not load')); };
      document.head.appendChild(s);
    });
  }

  function closeCheckout() {
    if (embeddedCheckout) { try { embeddedCheckout.destroy(); } catch (e) {} embeddedCheckout = null; }
    if (cardElement) { try { cardElement.destroy(); } catch (e) {} cardElement = null; }
    $('checkout-box').hidden = true;
    $('card-box').hidden = true;
    $('card-pay').hidden = true;
    $('card-error').textContent = '';
    $('checkout').innerHTML = '';
  }

  // A colour token as #rrggbb for Stripe's card control, which lives in a
  // frame of its own and takes no CSS variables (the Donate page's way).
  function tokenHex(name, fallback) {
    try {
      var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
      var c = document.createElement('canvas'); c.width = c.height = 1;
      var g = c.getContext('2d');
      g.fillStyle = '#010203'; g.fillStyle = v;
      if (!v || g.fillStyle === '#010203') return fallback;
      g.fillRect(0, 0, 1, 1);
      var p = g.getImageData(0, 0, 1, 1).data;
      return '#' + [p[0], p[1], p[2]].map(function (n) { return ('0' + n.toString(16)).slice(-2); }).join('');
    } catch (e) { return fallback; }
  }

  // The card line: Stripe's one-line card control and a Pay button, for a
  // gate that made a PaymentIntent (flow "card").  'more payment options'
  // opens Stripe's full form instead (openFullCheckout).
  var cardElement = null;
  function openCard(r, amount) {
    var stripe = window.Stripe(r.publishable_key || publishableKey);
    cardElement = stripe.elements().create('card', { style: {
      base: { color: tokenHex('--pl-ink', '#101010'), fontFamily: getComputedStyle(document.body).fontFamily || 'system-ui, sans-serif',
              fontSize: '16px', '::placeholder': { color: tokenHex('--pl-ink-dimmer', '#6f6e68') } },
      invalid: { color: tokenHex('--pl-status-fail', '#b00018') } } });
    $('checkout').innerHTML = '';
    $('card-amount').textContent = credits(creditsFor(amount)) + ' modeling credits for ' + dollars(amount) + (pot ? ', into the community pot' : '');
    $('checkout-box').hidden = false;
    $('card-box').hidden = false;
    cardElement.mount('#card-line');
    cardElement.on('change', function (e) { $('card-error').textContent = e.error ? e.error.message : ''; });
    var pay = $('card-pay');
    pay.hidden = false; pay.disabled = false; pay.textContent = 'Pay ' + dollars(amount);
    pay.onclick = function () {
      pay.disabled = true; pay.textContent = 'Paying\u2026'; $('card-error').textContent = '';
      stripe.confirmCardPayment(r.client_secret, { payment_method: { card: cardElement } }).then(function (res) {
        if (res.error) {
          $('card-error').textContent = res.error.message || 'The card was not accepted. Nothing was charged.';
          pay.disabled = false; pay.textContent = 'Pay ' + dollars(amount);
        } else if (res.paymentIntent && res.paymentIntent.status === 'succeeded') {
          checkout = r.checkout;
          closeCheckout();
          note('Paid. Adding the credits\u2026');
          confirmTopup();
        } else {
          $('card-error').textContent = 'The payment did not go through. Nothing was charged.';
          pay.disabled = false; pay.textContent = 'Pay ' + dollars(amount);
        }
      });
    };
    note('');
  }

  // what the price AMOUNT buys, from the offers on show
  function creditsFor(amount) {
    var offer = pot || {};
    var i = topupAmounts(offer).indexOf(amount);
    return i >= 0 ? creditsSold(offer, amount, i) : amount;
  }

  // Stripe's full form in the page: every way of paying the account offers.
  // The human check passed for the card line covers this second request.
  function openFullCheckout(amount) {
    note('Opening the payment options\u2026');
    ensureSession().then(function () {
      return Promise.all([api('topup', { session: session, amount_cents: amount, embedded: true }), loadStripeJs()]);
    }).then(function (results) {
      var r = results[0];
      if (r.error) { note(r.error); return; }
      keepWallet(r);
      if (!r.client_secret) return startHostedTopup(amount, true);
      return window.Stripe(r.publishable_key || publishableKey).initEmbeddedCheckout({ clientSecret: r.client_secret }).then(function (co) {
        embeddedCheckout = co;
        $('checkout-box').hidden = false;
        co.mount('#checkout');
        note('');
      });
    }).catch(function (e) { note('Could not open the payment options: ' + e); });
  }

  // The card form on this page (Stripe's embedded Checkout) when the
  // gate hands out a publishable key; otherwise Stripe's hosted page.
  // Every purchase spends the current Turnstile token, like every prompt.
  function takeToken() { var t = turnstileToken; resetTurnstile(); return t; }

  function startTopup(amount) {
    chosenAmount = amount;
    closeCheckout();
    if (!turnstileReady()) { note('Complete the human check first.'); return; }
    if (!publishableKey) return startHostedTopup(amount);
    note('Preparing the card form\u2026');
    var token = takeToken();
    ensureSession().then(function () {
      // the card line first; a gate that does not know it answers with
      // the full form, as before
      return Promise.all([api('topup', { session: session, amount_cents: amount, embedded: true, flow: 'card', turnstile: token }), loadStripeJs()]);
    }).then(function (results) {
      var r = results[0];
      if (r.error) { note(r.error); return; }
      keepWallet(r);
      if (!r.client_secret) return startHostedTopup(amount, true);
      if (r.flow === 'card') return openCard(r, amount);
      var stripe = window.Stripe(r.publishable_key || publishableKey);
      return stripe.initEmbeddedCheckout({ clientSecret: r.client_secret }).then(function (co) {
        embeddedCheckout = co;
        $('checkout-box').hidden = false;
        co.mount('#checkout');
        note('');
      });
    }).catch(function (e) { note('Could not open the card form: ' + e); startHostedTopup(amount, true); });
  }

  // COVERED: a card line or a full form was opened for this purchase a
  // moment ago, and the human check it passed covers this request too
  function startHostedTopup(amount, covered) {
    if (!covered && !turnstileReady()) { note('Complete the human check first.'); return; }
    note('Opening the payment page\u2026');
    var token = covered ? null : takeToken();
    ensureSession().then(function () {
      return api('topup', { session: session, amount_cents: amount, turnstile: token });
    }).then(function (r) {
      if (r.error) { note(r.error); return; }
      keepWallet(r);
      location.href = r.url;
    }).catch(function (e) { note('Could not start the payment: ' + e); });
  }

  function confirmTopup() {
    if (!checkout || !wallet) return Promise.resolve();
    return ensureSession().then(function () {
      return api('confirm', { session: session, wallet: wallet, checkout: checkout });
    }).then(function (r) {
      if (r.pot) renderPot(r.pot, r.spend); else renderSpend(r.spend);
      note(r.outcome === 'credited' ? (r.pot ? 'Thank you: your credits are in the pot, for everyone\'s builds.' : 'Thank you: your credits are in.') :
           r.outcome === 'already' ? 'That payment was already credited.' :
           r.outcome === 'unpaid' ? 'The payment has not completed yet; reload in a moment.' :
           'The payment could not be confirmed: ' + r.text);
      history.replaceState(null, '', pageUrl('session=' + session));
    }).catch(function () {});
  }

  var logCount = 0, builds = 0, sourceDirty = false, lastSource = null, viewerUrl = null, timer = null, viewerLoaded = false;
  // the model file the viewer last drew, and the one an edit began from
  var drawnSource = null, editBase = null;
  var rendered = false, viewerPrivate = false;
  window.addEventListener('pageshow', function (event) { if (event.persisted) viewerLoaded = false; });
  // Cloudflare Turnstile: rendered when the lab has a site key; each
  // token is single-use, so the widget is reset after every prompt.
  var turnstileKey = null, turnstileWidget = null, turnstileToken = null;

  function turnstileReady() { return !turnstileKey || !!turnstileToken; }

  function renderTurnstile() {
    if (!turnstileKey || !window.turnstile) return;
    $('turnstile').hidden = false;
    turnstileWidget = window.turnstile.render('#turnstile', {
      sitekey: turnstileKey,
      size: 'flexible',
      callback: function (token) {
        turnstileToken = token;
        $('build').disabled = potEmpty();
        $('quota').textContent = potEmpty() ? 'the community pot is empty' : '';
      },
      'expired-callback': function () { turnstileToken = null; },
      'error-callback': function (code) { turnstileToken = null; showError('prompt-error', 'The human check could not load (' + code + ').  Reload the page.'); }
    });
  }

  function resetTurnstile() {
    turnstileToken = null;
    if (turnstileWidget !== null && window.turnstile) window.turnstile.reset(turnstileWidget);
  }

  var siblingUrl = null, engineHere = null;

  //
  // Downloads: the model as a file, from the download door.  Fetched
  // rather than followed, so a refusal (no solids to write, credits
  // spent) is said on the page and not saved as a file; the owner's key
  // goes as the header, as on every door.  downloadQuery names what to
  // build: the live session or an archived session's replay.
  //

  var downloadQuery = null;

  function makeDownloads(formats) {
    var select = $('download');
    formats.forEach(function (f) {
      var option = document.createElement('option');
      option.value = f.format;
      option.textContent = f.label;
      select.appendChild(option);
    });
    if (!formats.length) return;
    select.addEventListener('change', function () {
      var format = select.value;
      select.value = '';
      if (format && downloadQuery) download(format);
    });
  }

  function offerDownloads(query) {
    downloadQuery = query;
    $('download-row').hidden = !query || $('download').options.length < 2;
  }

  function download(format) {
    var headers = {}, key = archiveId ? owners[archiveId] : ownerKey();
    if (key) headers['X-Prompt-Lab-Owner'] = key;
    downloadSays('Writing ' + format.toUpperCase() + '\u2026');
    fetch(doors + '/api/download?' + downloadQuery + '&format=' + encodeURIComponent(format), { headers: headers })
      .then(function (r) {
        if (!r.ok) return r.json().then(function (j) { throw new Error(j.error || ('download failed: ' + r.status)); });
        var name = (/filename="([^"]+)"/.exec(r.headers.get('Content-Disposition') || '') || [])[1] || ('model.' + format);
        var note = r.headers.get('X-Prompt-Lab-Note');
        return r.blob().then(function (blob) {
          var a = document.createElement('a');
          a.href = URL.createObjectURL(blob);
          a.download = name;
          document.body.appendChild(a);
          a.click();
          setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
          downloadSays(note ? name + ': ' + note + '.' : 'Saved ' + name + '.', note ? 20000 : 5000);
        });
      })
      .catch(function (e) { downloadSays(e.message, 20000, true); });
  }

  // what a download did, in a note over the page for a while
  var downloadTimer = null;
  function downloadSays(text, ms, error) {
    var el = $('toast');
    el.textContent = text;
    el.className = error ? 'failed' : '';
    el.hidden = false;
    clearTimeout(downloadTimer);
    if (ms) downloadTimer = setTimeout(function () { el.hidden = true; }, ms);
  }

  function loadConfig() {
    if (configLoaded) return Promise.resolve();
    return fetch(doors + '/api/config').then(function (r) { return r.json(); }).then(function (config) {
      configLoaded = true;
      if (!session) reach(true);
      engineHere = config.engine || null;
      // which engine this room runs, and the lab on the other one
      if (config.engine_label) {
        $('engine').textContent = config.engine_label; $('engine').hidden = false;
        $('status-engine').textContent = config.engine_label;
      }
      if (config.sibling_url) {
        siblingUrl = config.sibling_url;
        $('sibling').href = config.sibling_url + (lab.pinned ? '?skin=' + encodeURIComponent(lab.pinned) : '');
        $('sibling').textContent = titled(config.sibling_label || 'the other lab');
        $('sibling').hidden = false;
      }
      // a community pot shows before there is a session to spend from it
      if (config.pot) renderPot(config.pot);
      makeDownloads(config.downloads || []);
      if (config.browsing) {
        $('browse-live').hidden = $('browse-archive').hidden = $('about-browsing').hidden = false;
        if (browse === 'live') $('browse-live').className = 'current';
        if (browse === 'archive' || archiveId) $('browse-archive').className = 'current';
      }
      // nothing to build on a listing or an archived session: no widget
      if (browse || archiveId) return;
      turnstileKey = config.turnstile_site_key || null;
      if (!turnstileKey) return;
      $('build').disabled = true;
      $('quota').textContent = 'checking you are human\u2026';
      window.__turnstileLoaded = renderTurnstile;
      var script = document.createElement('script');
      script.src = 'https://challenges.cloudflare.com/turnstile/v0/api.js?onload=__turnstileLoaded&render=explicit';
      script.async = true; script.defer = true;
      document.head.appendChild(script);
    }).catch(function (e) {
      // an answer that is not a configuration is an older lab, with no
      // such door: carry on without a widget.  No answer at all is the
      // lab out of reach.
      if (e instanceof TypeError) { reach(false); if (!session) schedule(5000); }
    });
  }

  // A command's name, in the manner of the others: each word with a capital.
  function titled(text) {
    return String(text).replace(/(^|\s)([a-z])/g, function (m, space, letter) { return space + letter.toUpperCase(); });
  }

  function api(path, body) {
    var headers = body ? { 'Content-Type': 'application/json' } : {};
    if (ownerKey()) headers['X-Prompt-Lab-Owner'] = ownerKey();
    var options = body ? { method: 'POST', headers: headers, body: JSON.stringify(body) } : { headers: headers };
    return fetch(doors + '/api/' + path + (body ? '' : '?session=' + encodeURIComponent(session)), options)
      .then(function (r) { return r.json().then(function (j) { j._status = r.status; return j; }); });
  }

  // A door that names no session (the listings, the archive).
  function getJSON(path) {
    // this browser's key for an archived session it opened: a private
    // one answers only to it
    var key = archiveId && owners[archiveId];
    return fetch(doors + '/api/' + path, key ? { headers: { 'X-Prompt-Lab-Owner': key } } : {})
      .then(function (r) { return r.json().then(function (j) { j._status = r.status; return j; }); });
  }

  function showError(id, text) {
    var el = $(id);
    el.textContent = text || '';
    el.hidden = !text;
  }

  function ensureSession() {
    if (session) return Promise.resolve();
    return api('session', { wallet: wallet }).then(function (r) {
      if (r.error) { throw new Error(r.error); }
      session = r.session;
      if (r.owner) keepOwner(session, r.owner);
      if (r.pot) renderPot(r.pot, r.spend); else renderSpend(r.spend);
      history.replaceState(null, '', pageUrl('session=' + session));
    });
  }

  function formatTime(seconds) {
    var d = new Date(seconds * 1000);
    return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' });
  }

  function renderLog(log) {
    var list = $('log'), box = $('listener');
    for (var i = logCount; i < log.length; i++) {
      var li = document.createElement('li');
      li.className = log[i].kind;
      var t = document.createElement('time');
      t.textContent = formatTime(log[i].time) + ' \u00b7 ' + log[i].kind;
      li.appendChild(t);
      li.appendChild(document.createTextNode(log[i].text));
      list.appendChild(li);
    }
    $('welcome').hidden = log.length > 0;
    if (log.length > logCount) box.scrollTop = box.scrollHeight;
    logCount = log.length;
  }

  // Someone else's session, or an archived one: everything that would
  // change it goes, and a banner says whose it is.
  function readOnly(html) {
    editable = false;
    document.body.classList.add('read-only');
    $('prompt-form').hidden = true;
    $('spend-box').hidden = true;
    $('save-row').hidden = true;
    $('console-tab').hidden = true;
    source.lock(true);
    source.hint('No model file yet.');
    $('welcome').hidden = true;
    $('readonly-banner').innerHTML = html;
    $('readonly-banner').hidden = false;
    $('status-credits').textContent = '';
    status('Watching');
  }

  function formatDate(seconds) {
    return seconds ? new Date(seconds * 1000).toLocaleString([], { dateStyle: 'medium', timeStyle: 'short' }) : '';
  }

  function usageText(u, m, withCredits) {
    u = u || {}; m = m || {};
    return ((u.input || u.output) ? 'tokens: ' + u.input + ' in, ' + u.output + ' out, ' + u.cache_read + ' cached' : '')
      + ((m.compile || m.run) ? ((u.input || u.output) ? ' \u00b7 ' : '') + 'symbols: ' + m.compile + ' compiled, ' + m.run + ' run'
         + (withCredits && m.credits ? ' (' + credits(m.credits) + ' credits)' : '') : '');
  }

  function viewerTabUrl() {
    // a private session's viewer shows only to its key, in the tab too
    return dressed(viewerUrl + (viewerPrivate && editable && ownerKey() ? '&owner=' + encodeURIComponent(ownerKey()) : ''), false);
  }

  function render(state) {
    var log = state.log || [];
    reach(true);
    resumed = false;
    renderLog(log);
    if (state.editable === false && editable) {
      readOnly('You are watching someone else\u2019s session, opened ' + escapeHtml(formatDate(state.created))
        + '. It is read-only and follows along as they build. <a href="' + pageUrl('') + '">Start your own</a>.');
    }

    $('busy').hidden = !state.busy;
    var capped = !state.prompts_unlimited && state.prompts_used >= state.prompts_allowed;
    $('build').disabled = state.busy || capped || !turnstileReady();
    $('quota').textContent = state.busy ? 'working\u2026'
      : !turnstileReady() ? 'checking you are human\u2026'
      : state.prompts_unlimited ? (state.pot ? 'prompts draw on the community pot' : 'prompts draw on your credits')
      : (state.prompts_allowed - state.prompts_used) + ' of ' + state.prompts_allowed
        + (state.pot ? ' prompts left in this session' : ' free prompts left');
    $('usage').textContent = usageText(state.usage, state.meter, true);
    $('status-session').textContent = 'Session ' + state.session;
    if (editable) status(state.busy ? 'Run' : 'User Input');
    else status(state.busy ? 'Run' : 'Watching');
    if (state.pot) renderPot(state.pot, state.spend);
    else if (state.spend) renderSpend(state.spend);

    if (state.model_source !== lastSource) {
      lastSource = state.model_source;
      if (!sourceDirty) source.set(state.model_source || '');
    }
    // the agent wrote a new version under an edit in progress: say so,
    // and Save asks before it replaces that version
    if (sourceDirty && lastSource !== editBase) {
      $('source-state').textContent = 'the model changed since your edit began; Save replaces it';
    }
    $('save').disabled = state.busy || !sourceDirty;

    // closing the session to watchers, once it has bought credits
    if (editable) {
      $('privacy-row').hidden = !(state.may_be_private || state.private);
      if (!privacyPending) $('private').checked = !!state.private;
    }

    viewerUrl = state.viewer_url;
    viewerPrivate = !!state.private;
    $('viewer-tab').href = viewerTabUrl();
    $('viewer-tab').hidden = !state.model_defined;
    $('viewer-menu').hidden = !state.model_defined;
    offerDownloads(state.model_defined ? 'session=' + encodeURIComponent(state.session) : null);
    if (state.console_url) { $('console-tab').href = state.console_url; $('console-tab').hidden = false; }

    var completed = log.filter(function (e) { return e.kind === 'done' || e.kind === 'reload'; }).length;
    // A browser coming back from the payment page may restore the
    // iframe's old instance URL, which the server has since forgotten
    // (a 404 in the viewer); a fresh src on the first render fixes it.
    // A build that wrote the model and then stopped (the round cap, a
    // timeout, an API error) logs no done: the file the viewer drew is
    // the test then, once the session is idle.
    if (state.model_defined && (completed !== builds || $('viewer').hidden || !viewerLoaded
                                || (!state.busy && state.model_source !== drawnSource))) {
      drawnSource = state.model_source;
      // on a phone a model just built, or one found here on arrival,
      // takes the screen: it is what the visitor came for
      if (phone() && screen === 'prompt' && !checkout && (rendered ? completed !== builds : true)) show('model');
      viewerLoaded = true;
      builds = completed;
      // the owner's key rides on the iframe (it cannot send a header):
      // only the owner's draws are metered
      $('viewer').src = dressed(viewerUrl + (editable && ownerKey() ? '&owner=' + encodeURIComponent(ownerKey()) : ''), true) + '&t=' + Date.now();
      $('viewer').hidden = false;
      $('viewer-empty').hidden = true;
    }
    rendered = true;
    // what an app opened from its icon takes up again
    if (editable && ownerKey()) keepLast(state);
    // a watcher polls more gently
    schedule(state.busy ? (editable ? 2000 : 4000) : (editable ? 8000 : 15000));
  }

  function escapeHtml(s) {
    return String(s == null ? '' : s).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }


  //
  // An archived session: its log, each version of its model file, and
  // the model drawn in a replay (compiled again on the server).
  //

  function loadArchived(version) {
    return getJSON('archived?id=' + encodeURIComponent(archiveId) + (version ? '&version=' + version : ''))
      .then(function (a) {
        if (a._status !== 200) { readOnly(escapeHtml(a.error || 'This archived session could not be read.')); return null; }
        source.set(a.model_source || '');
        return a;
      });
  }

  function showArchive() {
    $('viewer-empty').textContent = 'Loading the archived session\u2026';
    loadArchived().then(function (a) {
      if (!a) { $('viewer-empty').textContent = 'Nothing to show.'; return; }
      readOnly('An archived session, opened ' + escapeHtml(formatDate(a.created)) + ' on the '
        + escapeHtml(a.engine) + ' engine. Read-only.'
        + (a.live ? ' It is still live: <a href="' + pageUrl('session=' + encodeURIComponent(a.id)) + '">follow it</a>.' : '')
        + ' <a href="' + pageUrl('') + '">Start your own</a>.');
      status('Archive');
      $('status-session').textContent = 'Session ' + a.id;
      renderLog(a.log || []);
      $('usage').textContent = usageText(a.usage, a.meter, false);
      if (a.versions > 1) {
        var select = $('versions');
        for (var v = 1; v <= a.versions; v++) {
          var o = document.createElement('option');
          o.value = v; o.textContent = v + (v === a.versions ? ' (last)' : '');
          select.appendChild(o);
        }
        select.value = a.versions;
        select.addEventListener('change', function () { loadArchived(select.value); });
        $('versions-row').hidden = false;
      }
      if (!a.replayable) {
        $('viewer-empty').innerHTML = !a.model_source ? 'This session left no model.'
          : siblingUrl && a.engine !== engineHere
            ? '<span>This model was built on the other engine. <a href="' + siblingUrl + '?archive=' + encodeURIComponent(a.id) + '">Draw it there</a>.</span>'
            : 'This model cannot be drawn here.';
        return;
      }
      $('viewer-empty').textContent = 'Building the model to draw it\u2026 a large one takes a while.';
      var key = owners[a.id] || null;
      return fetch(doors + '/api/replay', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ id: a.id, owner: key }) })
        .then(function (r) { return r.json(); })
        .then(function (r) {
          if (r.error || !r.ok) { $('viewer-empty').textContent = r.error || ('The model did not build again: ' + (r.text || '')); return; }
          var url = r.viewer_url + (key ? '&owner=' + encodeURIComponent(key) : '');
          $('viewer-tab').href = dressed(url, false); $('viewer-tab').hidden = false;
          $('viewer-menu').hidden = false;
          offerDownloads('replay=' + encodeURIComponent(a.id));
          $('viewer').src = dressed(url, true) + '&t=' + Date.now();
          $('viewer').hidden = false;
          $('viewer-empty').hidden = true;
          if (phone()) show('model');
        });
    }).catch(function (e) { $('viewer-empty').textContent = 'Could not load the archived session: ' + e; });
  }


  //
  // The listings: live sessions, and the archive by day.
  //

  // what a listing says of a model's need for solids (thumbs.lisp)
  var SOLIDS = {
    needs: ['needs solids', 'Built from solid bodies: it runs only in the solid modelling lab.'],
    benefits: ['better with solids', 'Runs without solid modelling, and is better with it: holes cut, parts joined, real volumes.'],
    none: ['no solids needed', 'Runs without solid modelling, and would gain nothing from it.']
  };

  function sessionItem(s, href) {
    var li = document.createElement('li');
    var a = document.createElement('a');
    a.href = href;
    a.setAttribute('data-doc', 'Open this session, read-only.');
    // the archive's thumbnail, dated so a new drawing is fetched anew
    var thumb = document.createElement('span');
    thumb.className = 'thumb';
    if (s.thumb) {
      var img = document.createElement('img');
      img.src = doors + '/api/thumb?id=' + encodeURIComponent(s.id) + '&v=' + s.thumb;
      img.alt = '';
      img.loading = 'lazy';
      img.decoding = 'async';
      thumb.appendChild(img);
    } else if (s.thumb === null) thumb.classList.add('none');
    if (s.thumb !== undefined) { a.classList.add('has-thumb'); a.appendChild(thumb); }
    var body = document.createElement('span');
    body.className = 'body';
    a.appendChild(body);
    var title = document.createElement('span');
    title.className = 'title' + (s.title ? '' : ' none');
    title.textContent = s.title || (s.model ? 'a model written by hand' : 'no prompt yet');
    body.appendChild(title);
    var meta = document.createElement('span');
    meta.className = 'meta';
    function pill(text, cls) { var p = document.createElement('span'); p.className = 'pill' + (cls ? ' ' + cls : ''); p.textContent = text; meta.appendChild(p); return p; }
    function text(t) { var p = document.createElement('span'); p.textContent = t; meta.appendChild(p); }
    if (owners[s.id]) pill('yours', 'mine');
    if (s.busy) pill('building now', 'live');
    else if (s.live) pill('live', 'live');
    pill(s.engine === 'solid' ? 'solid' : 'gendl');
    if (SOLIDS[s.solids]) pill(SOLIDS[s.solids][0], 'solids-' + s.solids).setAttribute('data-doc', SOLIDS[s.solids][1]);
    text(s.prompts + (s.prompts === 1 ? ' prompt' : ' prompts'));
    text('opened ' + formatDate(s.created));
    if (s.last_used && s.last_used !== s.created) text('last active ' + formatDate(s.last_used));
    body.appendChild(meta);
    li.appendChild(a);
    return li;
  }

  function showBrowse() {
    $('lab-main').hidden = true;
    $('browse-main').hidden = false;
    document.body.classList.add('browsing');
    var archive = browse === 'archive';
    // the lab's own name, as the server put it in the page
    var labTitle = document.title;
    document.title = archive ? labTitle + ' archive' : labTitle + ': live sessions';
    $('browse-title').textContent = archive ? 'The Archive' : 'Live Sessions';
    status(archive ? 'Archive' : 'Listing');
    $('browse-note').textContent = archive
      ? 'Every session opened here, newest first. Open one to read its log and each version of its model, and to see the model drawn.'
      : 'Sessions open now, most recently active first. Open one to watch it, read-only; only the visitor who opened a session can prompt or edit it.';
    getJSON(archive ? 'archive' : 'sessions').then(function (r) {
      var box = $('browse-list');
      if (r._status !== 200) { box.textContent = r.error || 'The list could not be read.'; return; }
      var sessions = r.sessions || [];
      if (!sessions.length) { box.textContent = archive ? 'The archive is empty.' : 'No session is open right now.'; return; }
      var ul = null, day = null;
      sessions.forEach(function (s) {
        var d = archive ? new Date((s.created || 0) * 1000).toLocaleDateString([], { dateStyle: 'full' }) : '';
        if (!ul || d !== day) {
          day = d;
          if (archive) { var h = document.createElement('h3'); h.textContent = d; box.appendChild(h); }
          ul = document.createElement('ul'); ul.className = 'sessions'; box.appendChild(ul);
        }
        ul.appendChild(sessionItem(s, pageUrl((archive ? 'archive=' : 'session=') + encodeURIComponent(s.id))));
      });
      if (archive && r.total > sessions.length) {
        var p = document.createElement('p'); p.className = 'muted';
        p.textContent = 'The newest ' + sessions.length + ' of ' + r.total + '.';
        box.appendChild(p);
      }
    }).catch(function (e) { $('browse-list').textContent = 'Could not reach the lab: ' + e; });
  }

  function schedule(ms) {
    clearTimeout(timer);
    timer = setTimeout(poll, ms);
  }

  function poll() {
    // no session yet, and the lab did not answer: ask again
    if (!session) { if (unreachable) loadConfig(); return; }
    api('state').then(function (state) {
      if (state._status === 404 && resumed) { reach(true); startAfresh(); return; }
      if (state._status === 403) {
        readOnly(escapeHtml(state.error || 'This session is private.') + ' <a href="' + pageUrl('') + '">Start your own</a>.');
        return;
      }
      if (state._status === 404) {
        showError('prompt-error', 'This session is gone (the server restarted, or it expired). Start a new one.');
        // its record is in the archive, when browsing is on
        if (!$('browse-archive').hidden) $('prompt-error').insertAdjacentHTML('beforeend', ' <a href="' + pageUrl('archive=' + encodeURIComponent(session)) + '">Read it in the archive</a>.');
        $('build').disabled = true;
        return;
      }
      render(state);
    }).catch(function () { reach(false); showLast(); schedule(5000); });
  }

  window.addEventListener('online', function () { if (unreachable) { clearTimeout(timer); poll(); } });

  $('prompt-form').addEventListener('submit', function (event) {
    event.preventDefault();
    var prompt = $('prompt').value.trim();
    if (!prompt) return;
    if (!turnstileReady()) { showError('prompt-error', 'Complete the human check first.'); return; }
    showError('prompt-error', '');
    $('build').disabled = true;
    var token = turnstileToken;
    ensureSession().then(function () {
      return api('prompt', { session: session, prompt: prompt, turnstile: token });
    }).then(function (r) {
      resetTurnstile();
      if (r.error) { showError('prompt-error', r.error); $('build').disabled = !turnstileReady() || potEmpty(); return; }
      $('prompt').value = '';
      fit($('prompt'));
      schedule(500);
    }).catch(function (e) { resetTurnstile(); showError('prompt-error', 'Could not reach the lab: ' + e); $('build').disabled = !turnstileReady() || potEmpty(); });
  });

  // On a phone the prompt's box is one line that grows with what is typed.
  function fit(box) {
    box.style.height = '';
    if (phone() && box.value) box.style.height = Math.min(box.scrollHeight + 2, 160) + 'px';
  }
  $('prompt').addEventListener('input', function () { fit($('prompt')); });

  // Control-Return (Command-Return) builds, from the keyboard.
  $('prompt').addEventListener('keydown', function (event) {
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey) && !$('build').disabled) {
      event.preventDefault();
      if ($('prompt-form').requestSubmit) $('prompt-form').requestSubmit(); else $('build').click();
    }
  });

  $('source').addEventListener('input', edited);
  $('fold-all').addEventListener('click', function () { if (editor) editor.foldAll(); });
  $('unfold-all').addEventListener('click', function () { if (editor) editor.unfoldAll(); });

  $('save').addEventListener('click', function () {
    if (sourceDirty && lastSource !== editBase
        && !window.confirm('The model file changed since your edit began (the agent wrote a new version). Save your edit over it?')) return;
    showError('source-error', '');
    $('save').disabled = true;
    api('model', { session: session, source: source.get() }).then(function (r) {
      if (r.error) { showError('source-error', r.error); $('save').disabled = false; return; }
      sourceDirty = false;
      $('source-state').textContent = r.ok ? 'saved and loaded' : 'saved; see the log';
      schedule(300);
    }).catch(function (e) { showError('source-error', 'Could not save: ' + e); $('save').disabled = false; });
  });

  $('new-session').addEventListener('click', function (event) {
    event.preventDefault();
    location.href = pageUrl('');
  });

  var privacyPending = false;
  $('private').addEventListener('change', function () {
    var want = $('private').checked;
    privacyPending = true;
    api('privacy', { session: session, private: want }).then(function (r) {
      privacyPending = false;
      if (r.error) { $('private').checked = !want; note(r.error); return; }
      if (r.owner) keepOwner(session, r.owner);
      $('private').checked = !!r.private;
      note(r.private ? 'This session is private now.' : 'This session is open to view again.');
      schedule(300);
    }).catch(function (e) { privacyPending = false; $('private').checked = !want; note('Could not change it: ' + e); });
  });

  $('checkout-close').addEventListener('click', function () { closeCheckout(); note(''); });
  // from the card line: Stripe's full form; from the full form: Stripe's own page
  $('checkout-hosted').addEventListener('click', function (event) {
    event.preventDefault();
    var fromCard = !!cardElement;
    closeCheckout();
    if (!chosenAmount) return;
    if (fromCard) openFullCheckout(chosenAmount); else startHostedTopup(chosenAmount, true);
  });


  //
  // The frame, the tabs, the commands.
  //

  $('tabs').addEventListener('click', function (event) {
    var button = event.target.closest && event.target.closest('button[data-screen]');
    if (button) show(button.getAttribute('data-screen'));
  });

  $('more').addEventListener('click', function () { sheet(!document.body.classList.contains('sheet-open')); });
  $('scrim').addEventListener('click', function () { sheet(false); });
  $('commands').addEventListener('click', function (event) {
    if (event.target.closest && event.target.closest('a')) sheet(false);
  });
  document.addEventListener('keydown', function (event) { if (event.key === 'Escape') sheet(false); });

  $('viewer-menu').addEventListener('click', function (event) {
    event.preventDefault();
    menusShown = !menusShown;
    if (screen !== 'model' && screen !== 'parts') show('model'); else syncFrame();
  });

  $('about-open').addEventListener('click', function (event) {
    event.preventDefault();
    var about = $('about');
    if (about.showModal) about.showModal(); else about.setAttribute('open', '');
  });
  // a click on the backdrop closes it
  $('about').addEventListener('click', function (event) { if (event.target === $('about')) $('about').close(); });

  $('viewer').addEventListener('load', function () {
    syncFrame();
    dressFrame();
    var doc = frameDocument();
    if (doc && doc.documentElement) { try { watchPointer(doc); } catch (e) {} }
  });

  // The browser's offer to install the lab, kept until the visitor asks
  // for it among the commands.
  var installOffer = null;
  window.addEventListener('beforeinstallprompt', function (event) {
    event.preventDefault();
    installOffer = event;
    $('install').hidden = false;
  });
  window.addEventListener('appinstalled', function () { installOffer = null; $('install').hidden = true; });
  $('install').addEventListener('click', function (event) {
    event.preventDefault();
    if (!installOffer) return;
    installOffer.prompt();
    installOffer.userChoice.then(function () { installOffer = null; $('install').hidden = true; });
  });

  // The worker that keeps the page's shell.  It is served beside the
  // page and reaches the page itself: the lab's prefix is its scope.
  if ('serviceWorker' in navigator && lab.worker && lab.prefix) {
    window.addEventListener('load', function () {
      navigator.serviceWorker.register(lab.worker, { scope: lab.prefix }).catch(function () {});
    });
  }

  function layoutChanged() { syncFrame(); fit($('prompt')); if (!phone()) sheet(false); }
  if (phoneQuery.addEventListener) phoneQuery.addEventListener('change', layoutChanged);
  else if (phoneQuery.addListener) phoneQuery.addListener(layoutChanged);

  // the editor's script is deferred: it has run by the time the document is parsed
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', mountEditor);
  else mountEditor();

  watchPointer(document);
  showDocumentation('');
  tick(); setInterval(tick, 30000);
  themeColor();
  makeSkinMenu();
  markLinks();

  if (browse) { loadConfig().then(showBrowse); return; }
  if (archiveId) { loadConfig().then(showArchive); return; }
  loadConfig();
  if (topupCancelled) { note('The purchase was cancelled; nothing was charged.'); history.replaceState(null, '', pageUrl(session ? 'session=' + session : '')); }
  if (checkout) confirmTopup().then(function () { if (session) poll(); });
  else if (session) poll();
})();
