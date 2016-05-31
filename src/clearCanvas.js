// module ClearCanvas

module.exports = {

    clearRect: function(canvasId) {
	return function() {
	    var c = document.getElementById(canvasId);
	    var ctx = c.getContext("2d");
	    ctx.clearRect(0,0,500,500);
	};
    },
    
    clearCanvas: function(canvasId) {
	return function() {
	    var c = document.getElementById(canvasId);
	    var ctx = c.getContext("2d");
	    ctx.clearRect(0,0,2000,2000);
	};
    }
    
};
