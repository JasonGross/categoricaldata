Raphael.el.draggable = function(set) {
  set = typeof(set) != 'undefined' ? set : r.set();
  this.dragWith = set;
  this.drag(dragUpdate, dragStart, dragStop);
}
Raphael.st.draggable = function(set) {
    this.forEach(function (el) {
        el.draggable(set);
    });
};

var dragging = {};
function dragStart() {
	   dragging.ox = this.attr('x');
	   dragging.oy = this.attr('y');
       dragging.x = 0;
       dragging.y = 0;
       dragging.active = true;
};                
function dragUpdate(dx, dy) {
	// TODO prevent dragging off the canvas.
	this.dragWith.translate( dx - dragging.x, dy - dragging.y );
	dragging.x = dx;
	dragging.y = dy;
};
function dragStop() {
	dragging.active = false;
};                      