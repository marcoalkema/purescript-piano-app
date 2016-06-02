
// module HeartBeat

module.exports = {


    loadHeartBeat : function(file){
	return function(send1){
	    return function(send2){
		return function(send3){
		    return function(){
						
			var sequencer = window.sequencer;
			
			sequencer.ready(function(){
			    
			    sequencer.addAssetPack({url: 'http://abumarkub.net/heartbeatjs/assets/asset_pack_basic.json'},

				   (function init(){
				       
				       sequencer.addMidiFile({url: file}, function() {
					   
					   var btnPlay = document.getElementById('Play_button');
					   var btnPause = document.getElementById('Pause_button');
					   var btnStop = document.getElementById('Stop_button');
					   var btnMetronome = document.getElementById('Metronome');
					   
					   var midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
					   var song = window.sequencer.createSong(midiFile);
					   var piano = sequencer.createInstrument('piano');

					   // Create handlers for UI Piano buttons
					   var keys = {};							   
					   var keyNames = Array.apply(null, Array(71)).map(function (_, i) {return ((i + 12));});
					   keyNames.map(function(n){
			    		       var key = "pianoKey".concat(n.toString());
					       var btnKey = document.getElementById(key);
					       btnKey.disabled = false;
					       keys[key] = btnKey;
					       
					       btnKey.addEventListener('mousedown', startNote, false);
					       btnKey.addEventListener('mouseup', stopNote, false);
					       btnKey.addEventListener('mouseout', stopNote, false);
					   });
					   
					   song.tracks.forEach(function(track){
					       track.setInstrument(piano);
					   });
					   
					   var track = song.tracks[0];
					   track.monitor = true;
					   track.setMidiInput('all');
					   track.setInstrument('piano');
					   
					   
					   function updateOnScreenKeyboard(event){
					       send2(event.data1)();
					   }
							   
					   function startNote(){
					       var noteNumber = sequencer.getNoteNumber(Number((this.id).split('pianoKey').join('')));
					       track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_ON, noteNumber, 100));
					   }
					   
					   function stopNote(){
					       var noteNumber = sequencer.getNoteNumber(Number((this.id).split('pianoKey').join('')));
					       track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_OFF, noteNumber));
							   }
					   
					   track.addMidiEventListener(sequencer.NOTE_ON, updateOnScreenKeyboard);
					   // TODO: Use note_off when UI handles lists of notes
					   // track.addMidiEventListener(sequencer.NOTE_OFF, updateOnScreenKeyboard);
					   
					   
					   //Handler for MIDI track
					   song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
					       send1(midiEvent.data1)();
					   });
					   
					   //Handler for end of track
					   song.addEventListener('end', function(midiEvent){
					       send3(true)();
					   });
					   
					   //HANDLERS FOR UI BUTTONS
					   btnPlay.addEventListener('click', function(){
					       song.play();
					   });
					   
					   btnPause.addEventListener('click', function(){
					       song.pause();
					   });
					   
					   btnStop.addEventListener('click', function(){
			    		       song.stop();
					   });
					   
					   btnMetronome.addEventListener('click', function(){
					       if(song.useMetronome === true){
						   song.useMetronome = false;
								   btnMetronome.value = 'metronome on';
					       }else if(song.useMetronome === false){
						   song.useMetronome = true;
						   btnMetronome.value = 'metronome off';
					       }
					   });
					   
				       });
				   })
						  );
			});
		    };
		};
	    };
	};
    }
};
			
					      
