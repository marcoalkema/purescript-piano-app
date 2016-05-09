// module HeartBeat

module.exports = {
    playNote : function(send){
	return function(){
	    var sequence = window.sequencer;
	    
	    sequence.ready(function init(){
		var song = sequence.createSong();
		
		if(sequence.midi === false){
		    console.log( 'No MIDI I/O');
		    return;
		}
		song.addMidiEventListener('note on', function(midiEvent){
		    console.log('Type: note_on: ' + midiEvent.data1);
		    send(midiEvent.data1)();
		});
	    });
	};
    }
};
