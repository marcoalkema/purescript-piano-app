
// module HeartBeat

module.exports = {


    loadHeartBeat : function(file){
	return function(send1){
	    return function(send2){
		return function(send3){
		    return function(){
			
			var btnPlay = document.getElementById('Play_button');
			var btnPause = document.getElementById('Pause_button');
			var btnStop = document.getElementById('Stop_button');
			var btnMetronome = document.getElementById('Metronome');

			
			var sequencer = window.sequencer;
			
			sequencer.ready(function(){
			    
			    sequencer.addAssetPack({url: 'http://abumarkub.net/heartbeatjs/assets/asset_pack_basic.json'},

						   (function init(){
						       
						       sequencer.addMidiFile({url: file}, function() {
							   
							   var midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
							   var song = window.sequencer.createSong(midiFile);
							   var piano = sequencer.createInstrument('piano');

							   song.tracks.forEach(function(track){
							       track.setInstrument(piano);
							   });

							   var track = song.tracks[0];
							   track.monitor = true;
							   track.setMidiInput('all');
							   track.setInstrument('piano');

							   // song.addMidiEventListener(sequencer.NOTE_ON, function(midi){
							   //     var noteNumber = sequencer.getNoteNumber(this.id);
							   //     send2(noteNumber)();
							   //     // track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_ON, noteNumber, 100));
							   // });
							   
							   //Handler for MIDI track
							   song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
							       send1(midiEvent.data1)();
							   });
							   
							   song.addEventListener('end', function(midiEvent){
							       console.log("EINDE");
							       send3(true)();
							   });
							   
							   //Handler for MIDI keyboard
							   song.addMidiEventListener('note on', function(midi){
							       var midiNote = midi.data1;							       
							       send2(midiNote)();
							   });
							   
							   //Handler for UI Piano
							   var arr = Array.apply(null, Array(71)).map(function (_, i) {return ((i + 12));});
							   arr.map(function(n){
			    				   var key = "pianoKey".concat(n.toString());
			    				       var pianoKey = document.getElementById(key);
							       
			    				   pianoKey.addEventListener('click', function(event){
							       console.log(event);
							       // var noteNumber = sequencer.getNoteNumber(this.id);
							       // track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_ON, 60, 100))();

			    				       var eventId = event.path[1].id;
			    				       var noteNumber = ((eventId.split('pianoKey').join('')));

							       var event1 = sequencer.createMidiEvent(0, sequencer.NOTE_ON, noteNumber, 100);
							       var event2 = sequencer.createMidiEvent(480, sequencer.NOTE_OFF, noteNumber, 0);
							       
							       console.log(event1);
							       console.log(track);
			    				       var events = sequencer.util.getRandomNotes({
			    				       	   minNoteNumber: Number(noteNumber) + 12,
			    				       	   maxNoteNumber: Number(noteNumber) + 12,
			    				       	   minVelocity: 100,
			    				       	   maxVelocity: 100,
			    				       	   noteDuration: 200, //ticks
			    				       	   numNotes: 1
			    				       });
							       console.log(events);
							       // track.processMidiEvent(event1);
			    				       sequencer.processEvents(event1, 120);
							       
			    				   });
							       
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
							   
							   // function matchNote(noteNumber){
							   // 	if (noteNumber == 0) {
							   // 	    return "c";
							   // 	};
							   // 	if (noteNumber == 1) {
							   // 	    return "c#";
							   // 	};
							   // 	if (noteNumber == 2) {
							   // 	    return "d";
							   // 	};
							   // 	if (noteNumber == 3) {
							   // 	    return "d#";
							   // 	};
							   // 	if (noteNumber == 4) {
							   // 	    return "e";
							   // 	};
							   // 	if (noteNumber == 5) {
							   // 	    return "f";
							   // 	};
							   // 	if (noteNumber == 6) {
							   // 	    return "f#";
							   // 	};
							   // 	if (noteNumber == 7) {
							   // 	    return "g";
							   // 	};
							   // 	if (noteNumber == 8) {
							   // 	    return "g#";
							   // 	};
							   // 	if (noteNumber == 9) {
							   // 	    return "a";
							   // 	};
							   // 	if (noteNumber == 10) {
							   // 	    return "a#";
							   // 	};
							   // 	if (noteNumber == 11) {
							   // 	    return "b";
							   // 	};				
							   // };
							   
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
			
					      
