// boxes stores all the current ontology boxes
var boxes = [];

// arrows stores all the current ontology arrows
var arrows = [];

// relations stores all the current ontology relations
var relations = [];
                
var ontology = {
		'boxes': [],
		'arrows': [],
		'relations': [],
		'display': { } 
}

function generateJSON() {
	// TODO
	// just take ontology and strip out display
}

function generateDSL() {
	// TODO
}

function loadJSON(json) {
	// first, check if json contains 'editor-state'
	// if note, rebuild it
	// now, reinitialize the canvas from the editor-state
}

function updateTextareas() {
	$('ontology-JSON').val(JSON.stringify(generateJSON()));
	$('ontology-DSL').val(generateDSL());
}

function createBoxRectangle(x, y) {
  var WIDTH = 100, HEIGHT = 50, ROUNDING = 5;
  var box = r.rect(x, y, WIDTH, HEIGHT, ROUNDING);
  var color = Raphael.getColor();
  box.attr({fill: color, stroke: color, "fill-opacity": 0.5, "stroke-width": 2, cursor: "move"});
  return box;
}
function addLabel(c, label) {
  label = typeof(label) != 'undefined' ? label : ''
  var OFFSET_x = c.getBBox().width / 2, OFFSET_y = c.getBBox().height / 2;
  return r.text(c.attr('x') + OFFSET_x, c.attr('y') + OFFSET_y, label);  
}

function createBox(x, y, label) {
  var box = createBoxRectangle(x, y);
    
  box.label = addLabel(box, label);
  
  box.addArrow = r.circle(0,0,3)
                  .transform(translationString(box.right()) + "t-10,0")                  // position just inside the right edge
                  .attr({ fill: '#ffffff', stroke: '#0000cc', 'stroke-width': 2 })       // blue, and filled (transparently)
                  .hover(function() { this.glowing = this.glow(); }, function() { this.glowing.remove(); });  // glow on hover
                  
  dragToCreateArrow(box.addArrow);

  box.receiveArrow = r.circle(0,0,3)
                  .transform(translationString(box.left()) + "t10,0")                    // position just inside the left
                  .attr({ fill: '#ffffff', stroke: '#0000cc', 'stroke-width': 2 })       // blue, and filled (transparently)
                  .hover(function() { this.glowing = this.glow(); }, function() { this.glowing.remove(); });  // glow on hover
                  // TODO make invisible (activate only while drawing an arrow

    
  var set = r.set()
  set.push(box, box.label, box.addArrow, box.receiveArrow);
  box.draggable(set);
  
  boxes.push(box);
  return box;
}               


// Basic dragging functionality, e.g. for boxes.
// x.draggable(set) makes x draggable, moving everything in set (optional) along too.

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

// Creating arrows via drag and drop
function dragToCreateArrow(source) {
	  var path;
	  var ox;
	  var oy;

	  function drawPath(dx, dy) {
		// TODO, make the path prettier: nicer curvatuare, color, thickness
	    path = r.path("M" + ox + "," + oy + "c20,0 -20,0 " + dx + "," + dy); 
	  }

	  function dragStart() {
		// record the initial coordinates of the component
	    var c = source.center();
	    ox = c.x;
	    oy = c.y;
	    
	    // activate the drop targets
	    // TODO
	    
	    drawPath(0, 0);
	  }
	  function dragUpdate(dx, dy) {
	    path.remove();
	    drawPath(dx, dy);
	  }
	  function dragStop() {
		// check if we're over a drop target, and if so, build a new arrow
		// TODO
		  
		// deactivate the drop targets
		// TODO
		  
	    path.remove();
	  }

	  source.drag(dragUpdate, dragStart, dragStop);
	  return source;
	}
