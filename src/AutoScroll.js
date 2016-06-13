// module AutoScroll

module.exports = {
    
    scrollTo : function (element, index) {
	var to = index * 50;
	var difference = to - element.scrollTop;
	
	setTimeout(function() {
            element.scrollTop = element.scrollTop + 10;
            if (element.scrollTop === to) return;
            scrollTo(element, index);}, 10);
    }
};
