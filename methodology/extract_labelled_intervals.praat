
select TextGrid sentences

numIntervals = Get number of intervals... 1

outdir$ = "C:\Users\michael\Dropbox\Michael_Dissertation\SentStimuli\Original\Sent3\"

for i from 1 to numIntervals
	select TextGrid sentences
	label$ = Get label of interval... 1 'i'

	if label$ <> ""
		begin = Get start point... 1 'i'

		begin = begin - 0.01
		end = Get end point... 1 'i'

		end = end + 0.01

		select LongSound sentences
		Extract part... 'begin' 'end' rectangular 1.0 0
		intervalfile$ = "'outdir$'" + "'label$'" + ".wav"
		indexnumber = 0
		while fileReadable (intervalfile$)
			indexnumber = indexnumber + 1
			intervalfile$ = "'outdir$'" + "'label$'" + "'indexnumber'" + ".wav"
		endwhile
		Write to WAV file... 'intervalfile$'
		Remove
	endif

endfor