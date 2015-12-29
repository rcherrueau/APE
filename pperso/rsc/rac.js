// -------------------------------------------- Utils
// http://fmdkdd.github.io/2014/03/28/functional-javascript-wizardry.html
var m2f = Function.prototype.bind.bind(Function.prototype.call);
var map = m2f(Array.prototype.map);
var reduce = m2f(Array.prototype.reduce);

// -------------------------------------------- Cmd Animation
// Constructs the cmd animation in a deferred object
function drawCmd(cmd, delay, id, dfr) {
    // Wraps each char of the cmd in a span
    var spans = map(cmd, function (char) {
        var span = $('<span />').html(char).css("display", "none");
        id.append(span);
        return span;
    });

    // Pipes the chain of animation and returns it
    return reduce(spans, function (dfr, span) {
        return dfr.pipe(function () {
            return span.delay(delay).fadeIn(0);
        });
    }, dfr);
}

// Blinks cursor animation in a defered object
function blinkCursor(id, delay, dfr) {
    return dfr.pipe(function () {
        return id.delay(delay).fadeOut(0)
                 .delay(delay).fadeIn(0);
    });
}

// Full cli animation
function cli(cursorId, cmdId) {
    var cmd = cmdId.html();
    var delay = 90;

    cmdId.empty();
    cmdId.removeClass("hidden");

    // Animation
    $.Deferred(function (dfr) {
        dfr = blinkCursor(cursorId, delay, dfr);
        dfr = blinkCursor(cursorId, delay, dfr);
        dfr = drawCmd(cmd, delay, cmdId, dfr);
        blinkCursor(cursorId, delay, dfr);
    }).resolve();
}

// -------------------------------------------- Others
function toggleNavFixed(size) {
    if (window.innerWidth < size) {
        $("#navbarblock").addClass("navbar-fixed-top");
    } else {
        $("#navbarblock").removeClass("navbar-fixed-top");
    }
}

// -------------------------------------------- Main
$(document).ready(function() {
    // Makes the navbar fixed if window size is under 768px
    var size = 768;
    toggleNavFixed(size);
    $(window).resize(function () { toggleNavFixed(size) });

    // Animates the cmd prompt
    cli($('#cli-cursor'), $('#cli-cmd'));
});
