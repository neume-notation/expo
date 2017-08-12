"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

/* This widget can display a video and text (optionally hiding the video)
 * such that the text scrolls with the time the video has reached, and, if
 * the video is paused, the text can be scrolled and the video is positioned
 * to match.  Additionally, a callback is called whenever nominated time
 * steps are reached - this callback is the `dispatcher`.
 *
 * To do this, the text must be structured with named markers which define
 * the positions to which it can be scrolled (sections) and other places
 * where the callback should be called.  The corresponding times are given
 * in an accompanying table.
 *
 * For this to work, the following rules must be observed:
 *
 *   - The text must be divided into sections, using <section> tags.
 *     No part of the text can be outside a section (but blank <div>s can
 *     be - see the discussion of "reset"s below); no nested sections
 *     are allowed.
 *
 *   - Each section should be small enough to fit comfortably within the
 *     the display area, as it cannot be scrolled more finely.
 *
 *   - Every <section> tag must have a `data-id` which is a `name` in the
 *     `timings` table, and the order of sections must be the same
 *     in both the text and the table.  These are used here to mark the time
 *     at which that part of the talk starts, and are passed to the
 *     dispatcher to determine whether any action is needed, and if so what.
 *
 *   - The `data-id` of the first <section> should be the first `name` in
 *     timings, and its `time` should be zero.  To be clear, no additional
 *     tag before the first section can be named in the table.
 *
 *   - Additional tags (typically <span>s or <div>s) appearing inside section
 *     text can be given `data-id`s which appear in the table as `name`s.
 *     The overall order of names in the table must match the order of the
 *     corresponding ids in the text.  Additional tags result in a call to
 *     the dispatcher at the relevant times.
 *
 *   - Certain named steps are nominated as "reset" actions for the purposes
 *     of the dispatcher.  This is indicated by a boolean flag in the
 *     `talkSequence` table, which contains the same names as `timings`.
 *     Usually a reset is needed at the start of a section, and, if there
 *     is no other action needed, it can be associated with the name of
 *     the section itself.  The first section of a talk is always is always
 *     marked as a reset like this, even if the associated action does not
 *     need to do anything.
 *
 *   - Additional names *within* a section must not be used for resets.
 *
 *   - If the name of the section is needed for an action other than reset,
 *     extra blank <div>s can appear *between* sections, with `data-id` names
 *     which are used for reset actions.  Generally the `name`s *within"
 *     a single section should have actions which accumulate sensibly (see
 *     the discussion of scrolling below).
 *
 *   - Sometimes a "partial reset" is needed when returning to the start of
 *     the same section - eg. it is generally correct to return highlights to
 *     the start-of-section state, but probably wrong to remove "More Details"
 *     buttons which have already been shown for that section.  In this
 *     situation two reset steps appear together: the first is the full
 *     reset (and is necessarily on an empty <div>), and the second is the
 *     partial reset (either on a div, or the section itself).  The partial
 *     reset is therefore the one found if the dispatcher is searching
 *     backwards for a reset, but otherwise, if time is advancing forwards,
 *     the full reset is done as well.
 *
 *   - Those methods which take a name to identify a position in the text
 *     generally want to find the section containing it.  For additional
 *     names *within* the text, it is indeed the containing <section> which
 *     is used.  If the name is outside a section, the next *following*
 *     section is used.
 *
 *   - By the preceding rules a reset cannot appear at the end of the talk.
 *     However, a named, empty <section> can be added at the end, and its
 *     name used for the reset.  This may come at the cost of a poor
 *     scroll position of the text at the very end of the talk.
 *
 *   - A handler callback may be registered to be told of changes arising
 *     from this widget, rather than those imposed on it by API calls.
 *     These are mainly video state changes: "ended" when the video reaches
 *     its end, and "paused" when scrolling positions the video before the
 *     end.  However, it is also called when the widget has re-calculated
 *     its sizing as a result of a window resize event.
 *
 * If the video is visible and running, the text will scroll automatically,
 * and manual scrolling is disabled.  It will scroll so as to centre the
 * section containing the latest `time` reported by the video, and this
 * section is highlighted (by using a lighter text colour).
 *
 * However, if the video is hidden, paused or finished the text *can* be
 * scrolled by any of the normal methods (a discrete scrollbar will be shown
 * when scrolling is available).  When this is done:
 *
 *   - The "active" section, marked by the `text-position` class, is made
 *     the one straddling the centre of the text container.
 *
 *   - The video is advanced to the `time` corresponding to the id of the
 *     *section* tag, ie to the *start* of the section.  (This might be
 *     done as part of the unhide/resume since program action may be
 *     needed there in any case - see below.)
 *
 *   - The callback is called in a way which actions all of the names
 *     contained *within* this section, in order.  This means the `data-id`
 *     of the section tag itself, and any `name`s which are `data-id`s of
 *     tags between the <section> and </section>.  (The idea is that, when
 *     scrolling, any visible changes accompanying the text should be
 *     applied for the *whole* section as a unit.)
 *
 * This code assumes that the containing HTML has class "fullheight" on some
 * ancestor whose height is to be taken as what is available.
 */

/* Constructor
 *
 * The parameters are:
 *     attach      An element (ie an actual DOM element) already in the DOM,
 *                 whose contents will be replaced by this component.
 *     spec        An object with these properties:
 *         video:  [ { url: <video url>, mimeType: <mime type> } ]
 *                 A list of {url, mimeType} records for the variant containers
 *                 codecs we want to support, in decreasing order of preference.
 *         text:   Html string
 *                 The HTML fragment giving the text, divided into sections as
 *                 described above.
 *         timing: [ { name: <id>, time: <millisecs> } ]
 *                 A table of names (ids in the HTML) and corresponding times
 *                 in the video - again note the rules above.
 *         startAt: An optional name (one of the `data-id` values) specifying a
 *                 a point at which to start (the default is the beginning!).
 *         vidHidden: Boolean
 *                 Whether to start with the video hidden.
 *     dispatcher  A Dispatcher object whose `dispatch` method will be used
 *                 to carry out actions associated with the position in the
 *                 text and/or video which has just been reached.
 */
function Narrative ( attach,     // html element we can put out HTML into
                     spec,       // spec object as just described
                     dispatcher  // Dispatcher object
                   ) {
    // Debug: uncomment to trace behaviour during development
    // console.log ("Create Narrative object");

    this.timings = spec.timing;
    this.dispatcher = dispatcher;

    // Quick lookup of name to give index in timings
    this.timeByName = {}
    for (var i = 0; i < spec.timing.length; i++) {
        this.timeByName[ spec.timing[i].name ] = i;
    }

    // Handlers for internally generated state changes - each will be called
    // with a string argument, currently either "paused" or "ended" referring
    // to the video, or "resize" referring to layout.
    this.handlers = [];

    // Variable used to throttle scroll callbacks during scroll
    // and similarly one for resize events
    this._whileScrollTimer = null;
    this._windowResizeTimer = null;

    // Timer used when the video is automatically paused because of the user
    // starting a scroll - at the timeout, it is automatically un-paused.
    this._pauseWhileScroll = null;

    // The scrollbar event namespace - see _setupScrollBar() and unAttach()
    this.mCSnamespace = "";

    // Set up dom, and keep the jQuery object
    this._dom = this._createDom (attach, spec.video, spec.text, spec.vidHidden);

    // The video element
    this._jqvid = this._dom.find ("video"); // jQuery wrapped
    this._video = this._jqvid.get(0);        // As a DOM object

    // Set up scrollbar on this component's text
    this._setupScrollBar ();

    // This must come after the scrollbar has been set up.
    this._setTalkContainerHeight ();
    var startSection = this._initialTextPos (spec.startAt);

    this._setupVideo ();

    // Explicitly call the first action, since by definition it is at time 0
    dispatcher.dispatch (spec.timing[0].name);

    // Index in timings of the last step actioned.  This is always
    // a valid index into timings
    this._lastTimeStep = 0;

    if (spec.startAt !== "") {
        // Carry out actions are carried out in the same way as if the
        // position had been changed by scrolling, and set the video time.
        this._actOnText (startSection.attr("data-id"));
    }

    // Detect window resize since we need to take some action of our own
    var self = this;
    this.resizeHandler = function () { self._windowResize (self); };
    $(window).resize (this.resizeHandler);

    // The video is not automatically started: playVideo must be called to do so
};

/* Play/resume the video.
 * If we are in an automatic pauseWhileScroll, don't do anything since the
 * video will start anyway after the timer expires.  If we are *not* in an
 * automatic pause, restart the video in a delayed fashion, by calling
 * _doPauseWhileScroll() - this ensures that if there is, in fact, a scroll
 * still under way, the restart is brought into the same mechanism as is used
 * when an automatic pause initiated by scrolling.
 */
Narrative.prototype.playVideo = function () {
    if (! this._jqvid.hasClass ("hidden") && this._pauseWhileScroll === null) {
        this._doPauseWhileScroll (true);
    }
};

/* Pause the video.
 * Cancel any pauseWhileScroll, since the pause is going to be real now.
 */
Narrative.prototype.pauseVideo = function () {
    if (! this._jqvid.hasClass ("hidden")) {
        this._stopPauseWhileScroll ();
        this._video.pause ();
        // Uncomment in un-minified merged code help get talk timings
        // alert ("Time: " + this._video.currentTime * 1000);
    }
};

/* Hide the video.
 * Cancel any pauseWhileScroll, since the pause is going to be real now.
 */
Narrative.prototype.hideVideo = function () {
    this._stopPauseWhileScroll ();
    this.pauseVideo ();
    this._jqvid.addClass ("hidden");
    this._initialTextPos (this.timings [this._lastTimeStep].name);
    // Do actions for the *whole* of the current section:
    this._actOnText (this.timings [this._lastTimeStep].name);
};

/* Unhide the video
 */
Narrative.prototype.unhideVideo = function () {
    this._jqvid.removeClass ("hidden");
    this._initialTextPos (this.timings [this._lastTimeStep].name);
    this.playVideo ();
};

/* Is the video currently hidden
 */
Narrative.prototype.isVideoHidden = function () {
    return this._jqvid.hasClass ("hidden");
};

/* Is the video currently playing, as opposed to paused or ended.
 * Count an active _pauseWhileScroll as playing, since it does not represent
 * any user intent, and the video will play again after the short timeout.
 */
Narrative.prototype.isVideoPlaying = function () {
    return ( this._pauseWhileScroll !== null ||
             ( ! this._video.paused && ! this._video.ended )
           );
};

/* The current time, in seconds, of the video
 */
Narrative.prototype.videoTime = function () {
    return ( this._video.currentTime );
};

/* Resize (or, more accurately, rearrange within the available container)
 */
Narrative.prototype.resize = function () {
    this._setTalkContainerHeight ();
    this._initialTextPos (this.timings [this._lastTimeStep].name);
};

/* Retrieve the vertical mid-point of the text (set by _setTalkContainerHeight).
 * This may be needed by a parent component to place buttons, etc.
 */
Narrative.prototype.textMidHeight = function () {
    return this._textMidHeight;
};

/* Remove the HTML from the DOM
 * To avoid the possibility (seen in testing) of callbacks continuing when
 * not expected, the dispatcher is replaced with a dummy, the video is paused,
 * and the scrollbar is removed before deleting the DOM elements.  To avoid
 * an undefined reference in mCustomScrollbar, the scrollbar is disabled
 * before it is destroyed.
 */
Narrative.prototype.unAttach = function () {
    var d = {};
    d.dispatch = function () {};
    this.dispatcher = d;
    this._stopPauseWhileScroll ();
    this._video.pause();
    var sb = this._dom.find (".talk-text").mCustomScrollbar;
    sb ("disable");
    sb ("destroy");
    $(window).off ("resize", this.resizeHandler);
    this._dom.empty ();

    // The scrollbar cleanup doesn't work properly, and leaves event handlers
    // accumulating at the document level.  The reason is not clear, but it
    // may be due to a race with removing this part of the DOM, since the
    // cleanup code still needs the selectors.  To work around this, the
    // namespace has been stored by _setupScrollBar (using inside knowledge
    // of the plugin) and we now remove any remaining events.
    if (this.mCSnamespace) {
        $(document).off ("." + this.mCSnamespace);
    }
};


/* Private functions */
/* ================= */

/* Find section from name */
/* ---------------------- */

// The returned value is the section itself if the name is that of a section,
// the containing section it it is the name of a tag within a section, or
// the immediately following section if the name appears on a tag between
// sections.
Narrative.prototype._enclosing = function (name) {
    var named = this._dom.find ("[data-id='" + name + "']");
    var section = named.closest ("section");
    if (section.length == 0) {
        section = named.nextAll ("section").first();
    }
    return section;
};


/* Create DOM */
/* ---------- */

Narrative.prototype._createDom = function (attach, videoUrls, text, hidden) {
    var width  = "640",
        height = "400";
    var html =   '<div class="narrative">\n'
               + '<video width="' + width + '" height="' + height
               +         (hidden ? '" class="hidden"' : '"') + '>\n';
    for (var i = 0; i < videoUrls.length; i++) {
        var v = videoUrls[i];
        html += '<source src="' + v.url + '" type="' + v.mimeType +'">\n';
    }
    html +=   'Sorry, your browser cannot play this video!\n'
            + '</video>\n'
            + '<div class="talk-container"><div class="talk-text">\n'
            + '<div class="talk-pad-top"></div>\n'
            + text
            + '<div class="talk-pad-bottom"></div>\n'
            + '</div><div class="talk-mask"></div></div>\n'
            + '</div>';
    $(attach).html (html);
    return $(attach).children (".narrative");
};


/* Setup Video */
/* ----------- */

/* This is initial setup, just after this component is created.  The
 * event handlers for "timeupdate" and "ended" are set up.
 */
Narrative.prototype._setupVideo = function () {
    var self = this;
    self._jqvid.on ("timeupdate", function (e) {
        if (! e.target.paused && ! e.target.ended) {
            // It seems that setting the video time in actOnText fires this
            // event!  So ignore events when the video is stopped.
            self._actOnTime (e.target.currentTime * 1000);
        }
    });
    self._jqvid.on ("ended", function (e) {
        if (self.lastTimeStep != self.timings.length - 1) {
            // Ensure all steps are completed, even if very close to (or
            // even beyond!) the end time of the video.
            self._actOnTime (self.timings[self.timings.length - 1].time)
        }
        self._widgetChange ("ended");
    });
};


/* Handler(s) for video state changes */
/* --------------------------------------- */

/* Register a handler
 */
Narrative.prototype.onWidget = function (handler) {
    this.handlers.push (handler);
};

/* Helper to call all handlers with the given state change and current
 * video time
 */
Narrative.prototype._widgetChange = function (reason) {
    for (var i = 0; i < this.handlers.length; i++) {
        this.handlers[i] ( {state: reason, time: this._video.currentTime});
    }
};


/* Call Action Dispatcher */
/* ---------------------- */

/* Perform actions, and synchronise text, to time `t` - driven by the video.
 * Time may go backwards (within a single section, actually) when the video
 * is resumed after a pause during which the user has scrolled.
 */
Narrative.prototype._actOnTime = function (t) {
    var fromIdx = this._backupIfNeeded (t);
    var toIdx   = this._latestStep (t, fromIdx);
    var forward = fromIdx == this._lastTimeStep;

    if (forward) {
        if (toIdx == this._lastTimeStep) {
            // No change - we are still at _lastTimeStep, going forwards
            return;
        } else {
            // Forward to a new target - in this case we have already done the
            // step at this._lastTimeStep, so advance fromIdx past it.
            fromIdx += 1;
        }
    }

    this.dispatcher.dispatch ( this.timings[toIdx].name,
                    this.timings[fromIdx].name,
                    ! forward
                  );
    this._lastTimeStep = toIdx;

    /* Note there is only one step of scrolling, even if there are several
     * actions, because the slow scoll whould be very bad if repeated several
     * times!
     */
    this._scrollTextTo (this.timings[this._lastTimeStep].name);
};

/* Perform actions after scrolling text, and set the video to the start of
 * the section.  The `name` parameter is the id of the section we have
 * arrived at, or specifies it by the rules of `_enclosing`.
 */
Narrative.prototype._actOnText = function (name) {

    var toName = name;  // Default if there are no sub-ids

    // `data-id`s of all elements *within" this section which have `data-id`s
    var ids = this._enclosing (name)
                       .find ( "[data-id]" )
                       .map (function () { return $(this).attr("data-id"); })
                       .get();

    // If there are any such ids, find the last one which is in `timings`,
    // and set `toName` to the corresponding name.  Normally the last element
    // of `ids` is the one, since there is no need for the sections of the text
    // contain any `data-id`s which are not in the table, but loop to play safe.
    for (var i = ids.length - 1; i >= 0; i--) {
        if (this.timeByName [ids[i]] !== undefined) {
            toName = ids[i];
            break;
        }
    }
    var toIdx = this.timeByName [toName];

    // Always reset to start of section, ie set `reset` parameter true
    this.dispatcher.dispatch (toName, name, true);
    this._lastTimeStep = toIdx;

    this._setVideoToSection (name);

};

/* Set the video to the named section.
 * This has no effect if the video is actually playing, but does operate
 * during any form of pause, *including* _pauseWhileScroll.  However, the
 * notification callback is *not* called (via _widgetChange) in the case
 * of a temporary pause indicated by _pauseWhileScroll.
 */
Narrative.prototype._setVideoToSection = function (name) {
    if (this._video && (this._video.paused || this._video.ended)) {
        // The allows the caller to determine that the pause/go control
        // should say something like "Resume" rather than "Play again".
        // (The belt-and-braces test of the video state is to minimise the
        // possibility of a race leaving the pause button in the wrong state.)
        var step = this.timeByName [name];
        if (step !== undefined) {
            this._video.currentTime = this.timings[step].time / 1000;
            if (this._pauseWhileScroll === null) {
                this._widgetChange ("paused");
            }
        }
    }
};

/* Find the latest step with time <= `t`, at or after `fromIdx`.
 */
Narrative.prototype._latestStep = function (t, fromIdx) {
    for (var i = fromIdx; i < this.timings.length; i++) {
        if (t < this.timings[i].time) {
            // `i` is the first step beyond `t`, so the one we want is `i-1`
            return Math.max (i-1, fromIdx);
        }
    }
    return this.timings.length - 1;  // `t` is past the last step in `timings`
};

/* Find the starting point for actions needed to catch up to time `t`.
 * The return value distinguishes between two cases:
 *
 *     1) result == this._lastTimeStep
 *        Time is still going forwards (or, to be precise, not backwards)
 *
 *     2) result < this._lastTimeStep
 *        Time has gone backwards, and the `result` is the index of the
 *        latest step whose time is no later than `t`, ie it is the last step
 *        which would have been actioned if time had gone monotonically
 *        upwards from 0 to `t`.  When going backwards to `t`, this step
 *        certainly needs to be actioned.  (The dispatcher may backtrack
 *        further, if it has a notion of "reset" steps.)
 */
Narrative.prototype._backupIfNeeded = function (t) {
    if (t < this.timings[this._lastTimeStep].time) {
        // Time has moved backwards!  Go back to the latest step whose
        // time is no later than `t`.  This should never need to go further
        // back than the step at 0, but force it explicitly to be sure.
        for (var i = this._lastTimeStep - 1; i >= 0; i--) {
            if (this.timings[i].time <= t || i == 0) {
                return i;
            }
        }
    } else {
        return this._lastTimeStep;
    }
};


/* Scrollbar setup */
/* --------------- */

Narrative.prototype._setupScrollBar = function () {
    var self = this;
    self._dom.find (".talk-text").mCustomScrollbar (
        { axis:             "y",  // Only "y"-axis scrolling (default actually)
          scrollButtons:     { enable: true },
          keyboard:          { enable: false },
          scrollbarPosition: "outside",
          scrollEasing:      "easeInOut",
          callbacks:         { onScroll: function () {
                                             self._updateScroll (this);
                                         },
                               whileScrolling: function () {
                                             self._whileScrolling (this);
                                         }
                             }
        }
    );

    // This is a hack using inside knowledge of mCustomScrollbar!
    // Get the event namespace used by this scrollbar for a belt-and-braces
    // clearing of handlers.  See the comment in unAttach.
    var d = self._dom.find (".talk-text").data ("mCS");
    self.mCSnamespace = "mCS_" + d.idx;
};

/* Callback at end of scroll.  This is a callback from mCustomScrollbar, so
 * the `e` value is defined by that package.
 */
Narrative.prototype._updateScroll = function (e) {
    if (this._video && (this._video.paused || this._video.ended)) {
        // Scrolling should automatically have paused the video, if it was
        // not already stopped. However, be defensive and refuse to act if
        // it is actually playing.
        var found = this._findVisibleSection(e).attr ("data-id");

        var scrollPos = - e.mcs.top;
        this._scrollTextTo (found, true, scrollPos);
            // Centre and highlight new active section
        if (this._pauseWhileScroll === null) {
            // Carry out actions if the video is truly paused or ended
            this._actOnText (found);
        } else {
            // Omit actions if there is a this._pauseWhileScroll timer,
            // because _actOnTime() will kick in as soon as the video is
            // restarted.  We must still position the video, which is
            // normally done in _actOnText().
            this._setVideoToSection (found);
        }
    }
    // Trigger something a handler attached at the document level can use
    // to detect activity, even though bubbling of the "wheel" event is
    // blocked by either mCustomScrollbar or jquery_mousewheel.
    this._dom.find (".talk-container").trigger ("scroll");
};

/* Callback during scroll to highlight the section which would become current
 * at the present position.
 * Throttled by timer to 10 per second
 */
Narrative.prototype._whileScrolling = function (e) {
    var self = this;
    if (! this._whileScrollTimer) {
        // Do actions only if timer has finished and removed itself
        this._doPauseWhileScroll (false);
        this._whileScrollTimer = setTimeout (function () {
                var found = self._findVisibleSection (e);
                if (! found.hasClass ("text-position")) {
                    $( ".text-position" ).removeClass ("text-position");
                    found.addClass ("text-position");
                }
                self._whileScrollTimer = null;  // Remove timer so we can act again
            },
            100   // 1/10 second delay
        );
    }
};

/* Automatically pause the video while scrolling, and set a short timout
 * after which it restarts.  The timeout is several times the throttling
 * delay in _whileScrolling() above, so as to ensure that, when scrolling
 * is continuing, this function is called again and renews the pause before
 * the _pauseWhileScroll timer expires.
 *
 * With `play` false, which is the way it is called from _whileScrolling(),
 * this has no effect while the video is explicitly paused.  However, when
 * playVideo is used to end an explicit pause, it invokes this with `play`
 * true, in order to start the video in a way which operates properly with
 * any scrolling which may be under way.
 */
Narrative.prototype._doPauseWhileScroll = function (play) {
    var self = this;
    if (this._pauseWhileScroll !== null) {
        clearTimeout (this._pauseWhileScroll);
    } else if (! play && (this._video.paused || this._video.ended)) {
        // When there is no existing _pauseWhileScroll timer, and the video is
        // paused or ended, it must mean that the video is genuinely stopped,
        // and restarting it after a timeout would be a mistake!  However,
        // this does not apply when `play` is true, since that is used when
        // the intent is to start playing.
        return;
    } else {
        this._video.pause ();
    }
    this._pauseWhileScroll =
        setTimeout (function () { self._video.play ();
                                  self._pauseWhileScroll = null;
                                },
                    500)
};

/* Call this when doing something which makes the restart irrelevant or wrong.
 */
Narrative.prototype._stopPauseWhileScroll = function () {
    if (this._pauseWhileScroll !== null) {
        clearTimeout (this._pauseWhileScroll);
        this._pauseWhileScroll = null;
    }
};


/* Text Scrolling by Section */
/* ------------------------- */

/* Set scroll position initially or on window resize.  If `name` is not
 * specified, the first section is chosen, otherwise the section whose `data-id`
 * is `name`, or is reached by `this._enclosing(name)`, is chosen.
 * Do not use _scrollTextTo(), since animation is wrong here - it would be
 * showing a slow scroll as we start the presentation!
 *
 * The return value is the jQuery object representing the section, which may
 * be useful to callers who wish to locate the containing section given the
 * name of an arbitrary step.
 */
Narrative.prototype._initialTextPos = function (name) {
    var section = name ? this._enclosing (name)
                       : this._dom.find ( "section" ).first();
    this._dom.find ( ".text-position" ).removeClass ("text-position");
    section.addClass ("text-position");
    var topOffset = this._activeTextTop (section);
    this._dom.find ( ".talk-text" ).mCustomScrollbar (
        "scrollTo",
        topOffset,
        {scrollInertia: 0, callbacks: false}
    );
    return section;
};

/* We are now using mCustomScrollbar, but if we wanted to return to doing
 * it our own way, we would position the .talk-text with something like
 * $( ".talk-text" ).animate ( {top: topOffset + "px"}, 1000 )
 *
 * The `userScroll` parameter is a boolean which is true if this method is
 * being called as a result of the user initiating the scroll.  It is false
 * if it is the result of the video timing.
 */
Narrative.prototype._scrollTextTo = function (name, userScroll, currentPos) {
    /* Enclosing section, either itself with this id or containing an
     * element with this id (or following a blank div with his name)
     */
    var section = this._enclosing (name);
    if (section.length == 0) {
        // This can happen if `name` is an additional marker between sections.
        // The text position is best left unchanged.
    } else if (section.hasClass ("text-position") && ! userScroll) {
        // Already the active section - don't do anything unless userScroll
    } else {
        this._dom.find ( ".text-position" ).removeClass ("text-position");
        section.addClass ("text-position");
        var scrollOptions = {callbacks: false, scrollEasing: "easeInOut"};
        if (userScroll) {
            // Try to make adjustment more natural
            if (currentPos && this._isUnMasked (section, currentPos)) {
                // Suppress unnecessary small adjustments, if the whole of the
                // section is within the unmasked area of the .talk-container
                return;
            }
            scrollOptions.scrollInertia = 2000;
            scrollOptions.scrollEasing = "easeInOutSmooth";
        }
        var topOffset = this._activeTextTop (section);
        this._dom.find ( ".talk-text" ).mCustomScrollbar (
            "scrollTo",
            topOffset,
            scrollOptions
        );
    }
};

/* Helper for both _scrollTextTo() and _initialTextPos() to find the distance
 * by which the ".talk-text" must be scrolled  (within a viewport the size
 * of the .talk-container) to put the centre of the section at the centre
 * of the viewport.
 *
 * The parameter is a jQuery object giving the section to be centred.
 */
Narrative.prototype._activeTextTop = function (section) {
    var pos = section.position(),
        mid = section.height() / 2;
    // So (pos.top+mid) is the vertical position of the mid-point of the
    // section relative to the .talk-text

    var centre = this._dom.find ( ".talk-container" ).height() / 2;

    // We want the mid-point of the section to be at the mid point of the
    // container:
    return (pos.top + mid) - centre;
};

/* A function (somewhat related to _activeTextTop) to determine whether the
 * given `section` (a jQuery object) is entirely within the unmasked area
 * of the .talk-container when scrolled by `currentPos` (which is positive,
 * ie the distance the viewport has been moved down relative to the content).
 */
Narrative.prototype._isUnMasked = function (section, currentPos) {
    var pos = section.position(),
        len = section.height();

    var tcHeight   = $( ".talk-container" ).height(),
        viewTop    = 0.1 * tcHeight,   // These fractions must agree with the
        viewBottom = 0.9 * tcHeight;   // CSS for .talk-mask (the 10% and 90%)

    return ( pos.top - currentPos > viewTop
             && pos.top - currentPos + len < viewBottom
           );
};

/* Helper for scroll callbacks
 * Finds the section straddling the centre of the viewport, returning it
 * as a jQuery object.  The `e` parameter is the value passed to callbacks
 * by mCustomScrollbar.
 */
Narrative.prototype._findVisibleSection = function (e) {
    var self       = this,
        scrollPos  = - e.mcs.top,
        height     = self._dom.find ( ".talk-container" ).height (),
        centreLine = scrollPos + height / 2;
    var found = self._dom.find ( "section" ).first();
    self._dom.find ( ".talk-text" ).find ("section").each (function () {
        if ($(this).position().top <= centreLine) {
            found = $(this);
        }
    });
    return found;
};


/* Height of Text Container */
/* ------------------------ */

/* Set the height of a .talk-container to fill what is left after the
 * video
 * FIXME the height of the bottom padding is not eactly right in
 * relation to user scrolling - scrolling to the bottom results in a small
 * adjustment to centre the last section (unless small adjustments are
 * suppressed in scrollTextTo when userScroll=true).
 */
Narrative.prototype._setTalkContainerHeight = function () {
    var tc = this._dom.find ( ".talk-container" ),
        tt = this._dom.find ( ".talk-text" ),
        padtop = this._dom.find ( ".talk-pad-top" ),
        padbot = this._dom.find ( ".talk-pad-bottom" ),
        cont = tc.closest (".fullheight");
    var bottomMargin = 120,   // Must agree with .expo-video CSS class
        newHeight = cont.height() - tc.position().top - bottomMargin;
    tc.height (newHeight);

    /* Set the padding at the top and bottom of the talk so that scrolling
     * to the top has the effect of centering the first section, and
     * likewise scrolling to the bottom centres the last section
    */
    var firstSection = tt.find ("section").first(),
        lastSection  = tt.find ("section").last(),
        viewport_2   = tc.height() / 2;
    padtop.height ( viewport_2 - firstSection.height() / 2);
    padbot.height ( viewport_2 - lastSection.height() / 2);

    tt.mCustomScrollbar ("update");

    this._textMidHeight = tc.position().top + newHeight / 2;
};


/* Window resize handler */
/* --------------------- */

/* This object has a handler for window resize events because it needs to
 * recalculate the text size and update the scrollbar when the window is
 * resized.  After doing the recalculation, this handler also calls any
 * registered handler for internally generated changes to allow the using
 * code to re-read the sizes if necessary.
 * Throttled by timer to 5 per second
 */
Narrative.prototype._windowResize = function (self) {
    if (! self._windowResizeTimer) {
        // Do actions only if timer has finished and removed itself
        self._windowResizeTimer = setTimeout (function () {
                self.resize();
                self._widgetChange ("resize");
                self._windowResizeTimer = null;  // Remove timer so we can act again
            },
            200   // 1/5 second delay
        );
    }
};

exports.Narrative = Narrative;
