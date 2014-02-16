/*!
 * \see http://paperjs.org/tutorials/geometry/vector-geometry/
 */
paper.install(window);

window.onload = function() {
  paper.setup('myCanvas');

  var point1 = new Point(50, 50);
  var point2 = new Point(110, 200);

  var x = point2.x - point1.x;
  var y = point2.y - point1.y;

  var p1ToP2 = new Point(point1.x + x, point1.y);

  var arrow1Item =
    new Group([
      new Path.Circle({
        center: point1,
        radius: 5,
        strokeColor: '#729fcf',
        strokeWidth: 2}),
      new Path({
        segments: [point1, p1ToP2],
        strokeColor: 'black',
        dashArray: [1, 2]}),
      new Path({
        segments: [[p1ToP2.x - 5, p1ToP2.y + 5],
                   p1ToP2,
                   [p1ToP2.x - 5, p1ToP2.y - 5]],
        strokeColor: 'black',
        dashArray: [1, 2]}),
      new PointText({
        point: [point1.x + 20, point1.y - 10],
        content: 'x: ' + x,
        fillColor: 'black'})]);

  var arrow2Item =
    new Group([
      new Path.Circle({
        center: point2,
        radius: 5,
        strokeColor: '#729fcf',
        strokeWidth: 2}),
      new Path({
        segments: [p1ToP2, point2],
        strokeColor: 'black',
        dashArray: [1, 2]}),
      new Path({
        segments: [[point2.x + 5, point2.y - 5],
                   point2,
                   [point2.x - 5, point2.y - 5]],
        strokeColor: 'black',
        dashArray: [1, 2]}),
      new PointText({
        point: [p1ToP2.x + 10, p1ToP2.y + 50],
        content: 'y: ' + y,
        fillColor: 'black'})]);
}
