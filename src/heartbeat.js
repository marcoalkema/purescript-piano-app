
// module HeartBeat

module.exports = {

    getSequencer : function(){
	var sequencer = window.sequencer;
	return sequencer;
    },

    getUserInput : function(send){
	return function(sequencer){
	    return function(){
		// var sequencer = window.sequencer;

		console.log("USERINPUT");
		
		sequencer.ready(function init(){
		    var song = sequencer.createSong();
		    if(sequencer.midi === false){
			console.log( 'No MIDI I/O');
		    }
		    song.addMidiEventListener('note on', function(midi){
			// console.log('Type: note_on: ' + midi.data1);
			send(midi.data1)();
		    });
		});
	    };
	};
    },
    
    
    // getCurrentNoteFromPlayback : function(send){
    // 	return function(sequencer){
    // 	    return function(){
    // 		console.log("MAUW");  
    // 		    // var track, song;
    // 		sequencer.ready(function init(){
    // 		    // track = sequencer.createTrack();
    // 		    // //track.setInstrument('Violin');
    // 		    // // track.setInstrument('piano');
    // 		    // // set monitor to true to route the incoming midi events to the track
    // 		    // track.monitor = true;
    // 		    // track.setMidiInput('all');
		    
    // 		    // song = sequencer.createSong({
    // 		    // 	tracks: track
    // 		    // });
		    
		
    // 		    var song = sequencer.createSong();
		    
    // 		    console.log(song);
		    
    // 		    if(sequencer.midi === false){
    // 			console.log( 'No MIDI I/O');
    // 		    }
    // 		    song.addMidiEventListener('note on', function(midi){
    // 			send(midi.data1)();
    // 			console.log('Type: note_on: ' + midi.data1);
			
    // 		    });

    // 		    song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
    // 			send(midiEvent.data1)();
    // 		    });
		    
    // 		});
    // 	    };
    // 	};
    // },
	
	loadFile : function(file){
	return function(send){
	    return function(sequencer){
		return function(){
		    var midiFile, song;
		    sequencer.ready(function init(){

		    	sequencer.addMidiFile({url: file}, function() {

		    	    midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
			    song = window.sequencer.createSong(midiFile);
			    
		    	    song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
				send(midiEvent.data1)();
			    });
			    song.addMidiEventListener('note on', function(midi){
				console.log('Type: note_on: ' + midi.data1);
				send(midi.data1)();
			    });
			});
		    });
		};
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
    },

    playNote : function(note){
	return function(sequence){
	return function(){
	    console.log("PLAYNOTE");
	    var sequencer = window.sequencer;
    
	    sequencer.ready(function init(){
	    	var song2,
	    	    part,
	    	    events;
		
	    	events = sequencer.util.getRandomNotes({
	    	    minNoteNumber: note,
	    	    maxNoteNumber: note,
	    	    minVelocity: 100,
	    	    maxVelocity: 100,
	    	    numNotes: 1
	    	});
		
		
	    	part = sequencer.createPart();
	    	part.addEvents(events);
		
	    	song2 = sequencer.createSong({
	    	    parts: part,
	    	    useMetronome: false
	    	});
	    	console.log(song2);
	    	song2.play();
	    });
	};
	};
    }
		   
};
