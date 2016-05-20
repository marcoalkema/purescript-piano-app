
// module HeartBeat

module.exports = {

    getCurrentNoteFromPlayback : function(send){
	return function(){
	    var sequencer = window.sequencer;
	    
	    sequencer.ready(function init(){
		var song = sequencer.createSong();
		
		if(sequencer.midi === false){
		    console.log( 'No MIDI I/O');
		}
		song.addMidiEventListener('note on', function(midi){
		    console.log('Type: note_on: ' + midi.data1);
		    send(midi.data1)();
		});
	    });
	};
    },

    loadFile : function(file){
	return function(send){
	    return function(){		
		var sequencer = window.sequencer;

		sequencer.ready(function init(){

		    sequencer.addMidiFile({url: file}, function() {
			var midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
			var song = window.sequencer.createSong(midiFile);
			song.play();
			
			song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
			    send(midiEvent.data1)();
			});
		    });
		});
	    };
	};
    },


    // loadFile : function(file){
    // 	return function(){
    // 	    var sequencer = window.sequencer;
    // 	    var midiFile;
    // 	    var song;
	    
    // 	    sequencer.addMidiFile({url: file}, function(){
    // 		midiFile = sequencer.getMidiFile(file.split('.mid').join(''));
    // 		song = sequencer.createSong(midiFile);
		
    // 	    });
    // 	};
    // },
    
    
    getMidiFile : function(file){
	return function(){
	    var midiFile;
	    midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
	    console.log(midiFile);
	    return midiFile;
	};
    },

    createSong : function(midiFile){
	return function(){
	    var song = window.sequencer.createSong(midiFile);
	    console.log(song);
	    return song;
	};
    },
    
    play : function(song){
	return function(){
	    console.log(song);
	    song.play();
	};
    },
    
    pause : function(song){
	return function(){
	    song.pause();
	};
    },

    stop : function(song){
	return function(){
	    song.stop();
	};
    }
};
