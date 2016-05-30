
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
		    // var btnStop = document.getElementById('Stop_button')
		    
		    var sequencer = window.sequencer;
		    
		    sequencer.ready(function init(){

			var createSlider = sequencer.util.createSlider;
			var sliderTempo;
			
			sequencer.addMidiFile({url: file}, function() {
    
			    var midiFile = window.sequencer.getMidiFile(file.split('.mid').join(''));
			    var song = window.sequencer.createSong(midiFile);
			    
			    
			    song.addEventListener('event', 'type = NOTE_ON', function(midiEvent){
				send1(midiEvent.data1)();
			    });
			    
			    song.addMidiEventListener('note on', function(midi){
				// console.log('Type: note_on: ' + midi.data1);
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
			    
			    // btnStop.addEventListener('click', function(){
			    // 	song.stop();
			    // });
			    
			    var arr = Array.apply(null, Array(71)).map(function (_, i) {return ((i + 12));});
			    arr.map(function(n){
				var key = "pianoKey".concat(n.toString());
				var pianoKey = document.getElementById(key);
				
			    pianoKey.addEventListener('click', function(event){
				
				var eventId = event.path[1].id;
				var noteNumber = ((eventId.split('pianoKey').join('')));
				var events = sequencer.util.getRandomNotes({
				    minNoteNumber: Number(noteNumber) + 12,
				    maxNoteNumber: Number(noteNumber) + 12,
				    minVelocity: 100,
				    maxVelocity: 100,
				    noteDuration: 200, //ticks
				    numNotes: 1
				});
				
				sequencer.processEvents(events, 120);
			    });
				
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
			    // var metronome = document.getElementById("metronomeWindow");

				// function (){
			    	//     var bpm;
			    	//     sliderTempo = createSlider({
			    	// 	slider: document.getElementById('tempo'),
			    	// 	min: 10,
			    	// 	max: 600,
			    	// 	step: 1,
			    	// 	message: 'tempo: {value}bpm',
			    	// 	onMouseMove: handle,
			    	// 	onMouseDown: handle,
			    	// 	onMouseUp: process
			    	//     });
				    
			    	//     sliderTempo.setValue(120);
			    	//     sliderTempo.setLabel(120);
				    
			    	//     function handle(value){
			    	//     	bpm = value;
			    	//     	sliderTempo.setLabel(value);
			    	//     }
				    
			    	//     function process(){
			    	//     	song.setTempo(bpm);
			    	//     }
				// };
			});
		    });
		};
		
	    };
	};
    }		   
};
