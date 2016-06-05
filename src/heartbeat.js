
// module HeartBeat

module.exports = {


    loadHeartBeat : function(file){
	return function(send1){
	    return function(send2){
		return function(send3){
		    return function(send4){
			return function(send5){
			    return function(send6){
				return function(){
				    
				    var sequencer = window.sequencer;
				    
				    sequencer.ready(function(){
					
					sequencer.addAssetPack({url: 'http://abumarkub.net/heartbeatjs/assets/asset_pack_basic.json'},
							       
							       (function init(){
				       
				       sequencer.addMidiFile({url: file}, function() {
					   
					   var btnPlay			= document.getElementById('Play_button');
					   var btnPause			= document.getElementById('Pause_button');
					   var btnStop			= document.getElementById('Stop_button');
					   var btnRecord		= document.getElementById('Record_button');
					   var btnMetronome		= document.getElementById('Metronome');
					   var btnLoop			= document.getElementById('Loop_button');
					   var metronomeSlider		= document.getElementById('metronomeSlider');
					   var leftLocatorSlider	= document.getElementById('leftLocator');
					   var rightLocatorSlider	= document.getElementById('rightLocator');

					   
					   
					   var midiFile			= window.sequencer.getMidiFile(file.split('.mid').join(''));
					   var song			= window.sequencer.createSong(midiFile);

					   var piano			= sequencer.createInstrument('piano');

					   var setRecord		= false;
					   var setMetronome		= false;
					   var setLoop			= false;

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

					   var reverb = sequencer.createReverb('simple_ir');
					   track.addEffect(reverb);
					   reverb.setAmount(0.4);

					   var track2 = sequencer.createTrack();
					   track2.monitor = true;
					   song.addTrack(track2);

					   track.movePart(song.tracks[0].parts[0], 960 * 4);
					   song.update();

					   
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
					       if ((setMetronome == false) && midiEvent.tick == 0) {
						   song.useMetronome = false;
					       }
					       if (setRecord == false) {
						   send1(midiEvent.data1)();
					       }
					       
					   });
					   
					   //Handler for end of track
					   song.addEventListener('end', function(midiEvent){
					       send3(true)();
					   });
					   
					   //HANDLERS FOR UI BUTTONS
					   btnPlay.addEventListener('click', function(){
					       song.stop();
					       if (setLoop == false){
						   song.setLoop(false);
					       }
					       else if (setLoop == true){
						   song.setLoop(true);
					       }
					       setRecord = false;
					       track.mute = false;
					       song.useMetronome = true;
					       song.play();
					   });
					   
					   btnPause.addEventListener('click', function(){
					       if (setRecord == false){
						   song.pause();
					       }
					   });
					   
					   btnStop.addEventListener('click', function(){
					       setRecord = false;
			    		       song.stop();
					   });

					   metronomeSlider.addEventListener('input', function(event){
					       var bpm = event.srcElement.value;
					       song.setTempo(bpm);
					       send4(Number(bpm))();
					   });

					   var canvas = document.getElementById("notationCanvas");
					   console.log(canvas);
					   canvas.addEventListener("input", function(event){
					       console.log("HOIHOIHOI");
					   });

					   btnRecord.addEventListener('click', function(){
					       setRecord = !setRecord;
					       song.stop();
					       if (setRecord == true) {
						   track.mute = true;
						   song.useMetronome = true;
						   song.setLoop();
						   song.setLeftLocator('barsbeats', 1,1,1,0);
						   song.setRightLocator('barsbeats', 10,1,1,0);
						   song.update();
						   song.play();
					       }
					       else if(setRecord == false){
						   track.mute = false;
						   song.update();
					       }
					   });

					   //BUGGY: Metronome is only off during record when clicked twice.
					   btnMetronome.addEventListener('click', function(){
					       setMetronome = !setMetronome;
					       if(song.useMetronome === true){
						   song.useMetronome = false;
						   btnMetronome.value = 'metronome on';
					       }else if(song.useMetronome === false){
						   song.useMetronome = true;;
						   btnMetronome.value = 'metronome off';
					       }
					   });

					   btnLoop.addEventListener('click', function(){
					       setLoop = !setLoop;
					       if (setLoop == true){
						   song.setLoop(true);
					       }
					       else if (setLoop == false){
						   song.setLoop(false);
					       }
					   });

					   leftLocatorSlider.addEventListener('input', function(event){
					       var loopStart = Number(event.srcElement.value);
					       song.setLeftLocator('barsbeats', loopStart + 1,1,1,0);
					       song.update();
					       send5(loopStart)();
					   });
					   
					   rightLocatorSlider.addEventListener('input', function(event){
					       var loopEnd = Number(event.srcElement.value);
					       song.setRightLocator('barsbeats', loopEnd + 1,1,1,0);
					       song.update();
					       send6(loopEnd)();
					   });
					   
				       });
							       })
							      );
				    });
				};
			    };
			};
		    };
		};
	    };
	};
    }
};
			
					      
