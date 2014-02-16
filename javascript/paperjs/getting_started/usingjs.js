// Copy over all fields from the paper object.
paper.install(window);

// Keep global references to both tools, so the HTML links below can
// access them.
var tool1, tool2;

window.onload = function() {
  paper.setup('myCanvas');

  // Create two drawing tools.
  // tool1 will draw straight lines,
  // tool2 will draw clouds.

  // Both share the mouseDown event:
  var path;

  function onMouseDown(event) {
    path = new Path();
    path.strokeColor = 'black';
    path.add(event.point);
  }

  tool1 = new Tool();
  tool1.onMouseDown = onMouseDown;
  tool1.onMouseDrag = function(event) {
    path.add(event.point);
  }

  tool2 = new Tool();
  tool2.minDistance = 20;
  tool2.onMouseDown = onMouseDown;
  tool2.onMouseDrag = function(event) {
    // Use the arcTo command to draw cloudy lines
    path.arcTo(event.point);
  }
}
