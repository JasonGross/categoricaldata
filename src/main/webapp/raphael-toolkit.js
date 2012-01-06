Raphael.el.bottomRight = function() {
  var bb = this.getBBox();
  return { x: bb.x + bb.width, y: bb.y + bb.height };
}
Raphael.el.right = function() {
  var bb = this.getBBox();
  return { x: bb.x + bb.width, y: bb.y + bb.height / 2 };
}
Raphael.el.center = function() {
  var bb = this.getBBox();
  return { x: bb.x + bb.width / 2, y: bb.y + bb.height / 2 };
}


function moveToString(coords) {
  return "T" + coords.x + "," + coords.y;
}
function translationString(coords) {
  return "T" + coords.x + "," + coords.y;
}
