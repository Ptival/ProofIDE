var cursorX = 0;
var cursorY = 0;

boundedDelta = function(value, delta, min, max) {
    result = value + delta;
    if (result < min) { return min; }
    else if (result > max) { return max; }
    else { return result; }
}

repositionCursor = function(deltax, deltay) {
    const lines = document.getElementsByClassName('pide-line');
    const nbLines = lines.length;
    const nbColumns = Math.max.apply(null, _.map(lines, function(e){return e.innerHTML.length}));
    cursorX = boundedDelta(cursorX, deltax, 0, nbColumns);
    cursorY = boundedDelta(cursorY, deltay, 0, nbLines);
    const cursor = document.getElementById('pide-cursor');
    const margin = document.getElementById('pide-margin');
    const cursorWidth = cursor.offsetWidth;
    const cursorHeight = cursor.offsetHeight;
    const cursorLeftOrigin = margin.offsetWidth;
    const cursorTopOrigin = 0;
    cursor.style.left = (cursorLeftOrigin + cursorX * cursorWidth) + "px";
    cursor.style.top = (cursorTopOrigin + cursorY * cursorHeight) + "px";
}

makeMargin = function(n) {
    margin = document.getElementById('pide-margin');
    _(_.range(n))
	.map(function(i){return i+1})
	.each(function(i){
	    marginLine = document.createElement('div');
	    marginLine.innerHTML = i;
	    margin.appendChild(marginLine);
	});
}

makeTextarea = function(n) {
    textarea = document.getElementById('pide-textarea');
    _(_.range(n)).each(function(){
	line = document.createElement('div');
	line.innerHTML = "Lorem ipsum";
	line.classList.add("pide-line");
	textarea.appendChild(line);
    });
}

blinkCursor = function() {
    switchy = true;
    const cursor = document.getElementById('pide-cursor');
    window.setInterval(function(){
	if (switchy) {
	    cursor.style.opacity = 1;
	} else {
	    cursor.style.opacity = 0;
	};
	switchy = !switchy;
    }, 500);
}

manageCursor = function() {
    // First place the cursor at its initial position
    const cursor = document.getElementById('pide-cursor');
    const margin = document.getElementById('pide-margin');
    cursorH = margin.offsetLeft + margin.offsetWidth;
    cursorV = 0;
    repositionCursor(0, 0);
    // Then handle arrow events
    document.onkeydown = function(e) {
	switch(e.keyCode) {
	    case 37: // ←
	    repositionCursor(-1, 0);
	    break;
	    case 38: // ↑
	    repositionCursor(0, -1);
	    break;
	    case 39: // →
	    repositionCursor(1, 0);
	    break;
	    case 40: // ↓
	    repositionCursor(0, 1);
	    break;
	}
    }
}

window.onload = function (){
    NB_LINES = 10;
    ide = document.getElementById('pide-container');
    makeMargin(NB_LINES);
    makeTextarea(NB_LINES);
    blinkCursor();
    manageCursor();
}
