# Flatplane vs Crossplane engine sound simulation
# www.overfitting.net
# https://www.overfitting.net/2022/06/simulacion-acustica-de-motores.html

library(tuneR)


# AUX FUNCTIONS

# Conversions time <-> samples. t=0 <-> n=1
time2sample=function(t, fs=24000) {round(t*fs+1)}
sample2time=function(n, fs=24000) {(n-1)/fs}

# Stroke pulse model
explode=function(t, A=1, t0=0, tmax=0.01, f=100, phi=0)
    {A*(t-t0)/tmax*exp(1-(t-t0)/tmax)*sin(2*pi*f*(t-t0)+phi)}
explode.env=function(t, A=1, t0=0, tmax=0.01)
    {A*(t-t0)/tmax*exp(1-(t-t0)/tmax)}


# ENGINE PARAMS
rpm=1000
strokepm=2*rpm
expps=strokepm/60
Tstroke=1/expps  # stroke period (0.03s)
f=167.364757  # pulse fundamental frequency (Hz)


# SINGLE STROKE
time=4*0.03  # allow stroke pulse to fade through 0.12s
t=seq(0, time, length.out=time2sample(time))  # t are seconds
stroke=explode(t, f=f)
stroke.env=explode.env(t)
strokelen=length(stroke)

# Draw single stroke pulse
plot(t, stroke, type='l', ylim=c(-1,1), xlab='t(s)', ylab='')
lines(t, stroke.env, lty='dotted', col='red')
lines(t,-stroke.env, lty='dotted', col='red')
abline(h=0)


# BUILD ENGINE SOUND
N=130*4  # total number of strokes (multiple of 4)
time=N*Tstroke+sample2time(strokelen)
freqs=24000
JITTER=2  # 0
ACCEL=1.0005  # 1

for (motor in c('flatplane', 'crossplane')) {
    Tstroke=1/expps  # reset to starting Tstroke
    engine=seq(0, 0, length.out=time2sample(time)) 
    
    initsample=1
    for (i in 1:N) {
        stroke=explode(t, f=f+rnorm(1)*JITTER)  # add jitter
        
        rango=initsample:(initsample+strokelen-1)
        if (motor=='crossplane' & ((i+2)%%4==0 | (i+1)%%4==0)) {
            # Crossplane delays 2nd and 3rd strokes by Tstroke/2
            rango=rango+time2sample(Tstroke/2)
        }
        engine[rango]=engine[rango]+stroke  # add stroke to waveform
        initsample=initsample+time2sample(Tstroke)  # shift to next stroke
        
        # Modulate rpm
        if (i<=4*N/5) {
            Tstroke=Tstroke/ACCEL  # slow speed up
        } else {
            Tstroke=Tstroke*ACCEL^15  # quick slow down
        }
    }
    print(paste0(motor, " engine Min/Max values: ",
                 min(engine), "/", max(engine)))
    engine=engine/max(abs(min(engine)), max(engine))  # normalize to -1/1
    
    
    # SAVE .WAV
    sound=readWave("sawtooth_bandlimited.wav")
    fs=as.numeric(sound@samp.rate)
    fs=48000
    bits=16
    
    sound@left=round(engine*(2^(bits-1)-1))  # normalize to 16-bit integer
    sound@samp.rate=fs
    sound@bit=bits
    writeWave(sound, filename=paste0("engine_", motor, "_",rpm,".wav"))

    
    # FFT ANALYSIS
    waveform=sound@left
    
    dft=abs(fft(waveform))
    NFFT=round(length(dft)/2)  # first half of FFT
    maxfreq=freqs/2/1000  # FFT max freq in kHz
    plot(seq(from=0, to=maxfreq, len=NFFT),
         dft[1:NFFT]/max(dft), main='Motor',
         xlab='f (kHz)', ylab='Amplitude', col='red', type='l')
    axis(side=1, at=c(0:maxfreq))
    
    png(width=512, height=400, paste0("engine_", motor, "_",rpm,".png"))
    CROP=30
    plot(seq(from=0, to=maxfreq/CROP, len=round(NFFT/CROP)),
         dft[1:round(NFFT/CROP)]/max(dft),
         main=paste0(motor, " engine at ",rpm," rpm"),
         xlab='f (kHz)', ylab='Amplitude', col='red', type='l')
    abline(v=f/1000, col='lightgray', lty='dotted')
    dev.off()
}



# MAKE VIDEO

# SAVE FINAL 16s .WAV
sound=readWave("engine_flatplane_1000.wav")
fs=as.numeric(sound@samp.rate)
length(sound@left)/fs  # duration
sound@left=c(sound@left,seq(0,0,
    length.out=time2sample(8-sample2time(length(sound@left), fs=fs), fs=fs)))
writeWave(sound, filename=paste0("engine_flatplane_1000_8s.wav"))

sound2=readWave("engine_crossplane_1000.wav")
fs=as.numeric(sound2@samp.rate)
length(sound2@left)/fs  # duration
sound2@left=c(sound2@left,seq(0,0,
    length.out=time2sample(8-sample2time(length(sound2@left), fs=fs), fs=fs)))
writeWave(sound2, filename=paste0("engine_crossplane_1000_8s.wav"))

sound@left=c(sound@left,sound2@left)
writeWave(sound, filename=paste0("flatplane_crossplane_1000_real_16s.wav"))


# ffmpeg
ffmpeg -loop 1 -framerate 1 -i ciguenal%3d.png
    -i flatplane_crossplane_1000_16s.wav -t 16
    -c:v libx264 -crf 15 -pix_fmt yuv420p flatplanecrossplane.mp4

ffmpeg -loop 1 -framerate 1 -i ciguenal%3d.png
    -i flatplane_crossplane_1000_real_16s.wav -t 16
    -c:v libx264 -crf 15 -pix_fmt yuv420p flatplanecrossplanereal.mp4
