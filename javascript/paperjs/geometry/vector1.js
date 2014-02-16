/*!
 * \see http://paperjs.org/tutorials/geometry/vector-geometry/
 */
paper.install(window);

window.onload = function() {
  paper.setup('myCanvas');

  var point1 = new Point(50, 50);
  var point2 = new Point(110, 200);

  var pointItem1 = new Group([
    new Path.Circle({
      center: point1,
      radius: 5,
      strokeColor: '#729fcf',
      strokeWidth: 2}),
    new PointText({
      point: new Point(point1.x + 10, point1.y + 2.5),
      content: 'point1\n{ x: ' + point1.x + ', y: ' + point2.y + '}',
      fillColor: 'black'})]);

  var pointItem2 = new Group([
    new Path.Circle({
      center: point2,
      radius: 5,
      strokeColor: '#729fcf',
      strokeWidth: 2}),
    new PointText({
      point: new Point(point2.x + 10, point2.y + 2.5),
      content: 'point2\n{ x: ' + point2.x + ', y: ' + point2.y + '}',
      fillColor: 'black'})]);
}
