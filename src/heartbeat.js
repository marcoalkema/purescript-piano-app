
// module HeartBeat

module.exports = {


    loadHeartBeat : function(file){
	return function(send1){
	    return function(send2){
		return function(){

		    var btnPlay = document.getElementById('Play_button');
		    var btnPause = document.getElementById('Pause_button');
		    // var btnPause = document.getElementById('Stop_button');
		    var btnMetronome = document.getElementById('Metronome');
		    var btnStop = document.getElementById('Stop_button');
		    
		    var sequencer = window.sequencer;

		    sequencer.addMidiFile({url: 'http://abumarkub.net/heartbeatjs/assets/minute_waltz.mid'});
		    // sequencer.addAssetPack({url: 'http://abumarkub.net/heartbeatjs/assets/asset_pack_basic.json'}, init);
		    
		    sequencer.ready(function init(){
			
			sequencer.addMidiFile({url: file}, function() {
			    
			    var midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
			    var song = window.sequencer.createSong(midiFile);
			    var piano = sequencer.createInstrument('piano');
			    
			    
			    song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
				send1(midiEvent.data1)();
			    });
			    
			    song.addMidiEventListener('note on', function(midi){
				console.log('Type: note_on: ' + midi.data1);
				var midiNote = midi.data1;
				var events = sequencer.util.getRandomNotes({
				    minNoteNumber: midiNote + 12,
				    maxNoteNumber: midiNote + 12,
				    minVelocity: 100,
				    maxVelocity: 100,
				    noteDuration: 200, //ticks
				    numNotes: 1
				});
				
				sequencer.processEvents(events, 120);
				
				send2(midiNote)();
			    });
			    
			    btnPlay.addEventListener('click', function(){
				song.play();
			    });
			
			    btnPause.addEventListener('click', function(){
				song.pause();
			    });
			    
			    btnStop.addEventListener('click', function(){
			    	song.stop();
			    });
			    
			    var arr = Array.apply(null, Array(71)).map(function (_, i) {return ((i + 12));});
			    arr.map(function(n){
				var key = "pianoKey".concat(n.toString());
				var pianoKey = document.getElementById(key);
				
				pianoKey.addEventListener('mousedown', startNote, false);
				pianoKey.addEventListener('mouseup', stopNote, false);
				pianoKey.addEventListener('mouseout', stopNote, false);

				track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_ON, noteNumber, 100));    
				
				// var eventId = event.path[1].id;
				// var noteNumber = ((eventId.split('pianoKey').join('')));
				// var events = sequencer.util.getRandomNotes({
				//     minNoteNumber: Number(noteNumber) + 12,
				//     maxNoteNumber: Number(noteNumber) + 12,
				//     minVelocity: 100,
				//     maxVelocity: 100,
				//     noteDuration: 200, //ticks
				//     numNotes: 1
				// });
				
				// sequencer.processEvents(events, 120);
				
			    });
			    
			    btnMetronome.addEventListener('click', function(){
				console.log("CLICKED");
				if(song.useMetronome === true){
				    song.useMetronome = false;
				    btnMetronome.value = 'metronome on';
				}else if(song.useMetronome === false){
				    song.useMetronome = true;
					btnMetronome.value = 'metronome off';
				}
			    });

			    var track = song.tracks[0];
			    track.monitor = true;
			    track.setMidiInput('all');
			    track.setInstrument('piano');

			    track.addMidiEventListener(sequencer.NOTE_ON, function(midiNote){
				console.log(midiNote);
				startNote();
				send2(midiNote.data1);

			    });
			    // track.addMidiEventListener(sequencer.NOTE_OFF, updateOnScreenKeyboard);

			    function getNote(pianoKey){
				var eventId = pianoKey.path[1].id;
				var noteNumber = ((eventId.split('pianoKey').join('')));
				var note = Number(noteNumber) % 12;
				return matchNote(note);
			    };

			    function matchNote(noteNumber){
				if (noteNumber == 0) {
				    return "c";
				};
				if (noteNumber == 1) {
				    return "c#";
				};
				if (noteNumber == 2) {
				    return "d";
				};
				if (noteNumber == 3) {
				    return "d#";
				};
				if (noteNumber == 4) {
				    return "e";
				};
				if (noteNumber == 5) {
				    return "f";
				};
				if (noteNumber == 6) {
				    return "f#";
				};
				if (noteNumber == 7) {
				    return "g";
				};
				if (noteNumber == 8) {
				    return "g#";
				};
				if (noteNumber == 9) {
				    return "a";
				};
				if (noteNumber == 10) {
				    return "a#";
				};
				if (noteNumber == 11) {
				    return "b";
				};				
			    };

			    function getOctave(pianoKey){
				var eventId = pianoKey.path[1].id;
				var noteNumber = ((eventId.split('pianoKey').join('')));
				// return 
			    }
			    
			    function startNote(){

				track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_ON, noteNumber, 100));
			    }
			    
			    function stopNote(){
				var pianoKey = sequencer.getNoteNumber(this.id);
				var noteNumber = ((pianoKey.split('pianoKey').join('')));
				track.processMidiEvent(sequencer.createMidiEvent(0, sequencer.NOTE_OFF, noteNumber));
			    }
			    

			});
		    });
		};
		
	    };
	};
    }		   
};
