// module HeartBeat

module.exports = {

    songEventListener: function(callback){
	return function() {
	    song.addEventListener('event', 'type = NOTE_ON', function(event){
		callback(event);		
	    });
	};
    }

    
};
