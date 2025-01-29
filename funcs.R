# Functions for WhistleGen.R 
# Should be located in the same directory as the 'WhistleGen.R' file


# Function to Create the actual whistle
create.triplet.signal <- function(freq1, freq2, freq3, dir, triplet.name, sample.rate, duration, noise.before, noise.after, whistle.tapering, tapering.duration) {
  
  # Calculate the length of the noise segments in samples
  noise.before.samples <- round((noise.before / 1000) * sample.rate)
  noise.after.samples <- round((noise.after / 1000) * sample.rate)
  
  # Generate noise before the whistle with a frequency content similar to the signals
  ### Adjust Amplitude of noise. 0 = no noise
  noise_before_sound <- generate_filtered_noise(noise.before.samples, sample.rate, freq1, freq2, freq3, amplitude = 0.1)
  
  # Create the whistle sound with fading
  triplet <- soundgen(pitch = list(time = c(0, 0.22, 0.38), # Adjust these to fix contour symmetry ######
                                   value =c(freq1, freq2, freq3)),
                      sylLen = duration,
                      rolloff = -20, # This line indicates the dB rolloff of the harmonics
                      #rolloffExact = 1, # Put a "#" on this line to include harmonics
                      noise = -25, ### Adjust noise behind signals. 0 = max dB
                      samplingRate = sample.rate,
                      pitchSamplingRate = sample.rate,
                      pitchCeiling = 50000,
                      temperature = 1e-6,
                      formants = NULL,
                      plot = FALSE, 
                      heights = c(0.5, 0.5),
                      addSilence = 0,
                      attackLen = c(whistle.tapering, whistle.tapering),
                      ampl = list(time = c(0, 0.01, 1),  # Don't change these time stamps
                                  value = c(0, 0, 0)), ### Only adjust 2nd and 3rd numbers, the 1st should always be 0. 
                      invalidArgAction = 'ignore')
  
  # Trim the first 40 milliseconds of the triplet signal ########
  trim_samples <- round((40 / 1000) * sample.rate)
  triplet <- triplet[(trim_samples + 1):length(triplet)]
  
  sig_tapering <- round((10 / 1000) * sample.rate)
  
  # Apply manual tapering of 10ms to the front of the triplet signal
  triplet[1:sig_tapering] <- triplet[1:sig_tapering] * seq(0, 1, length.out = sig_tapering)
  
  # Generate noise after the whistle with a frequency content similar to the signals
  # Adjust Amplitude of noise. 0 = no noise
  noise_after_sound <- generate_filtered_noise(noise.after.samples, sample.rate, freq1, freq2, freq3, amplitude = 0.1)
  
  # Tapering duration in samples
  ### NOTE: adjust the numerator to increase/decrease tapering
  taper_samples <- round((tapering.duration / 1000) * sample.rate)
  
  # Apply fade-out to the end of noise_before_sound
  noise_before_sound[(length(noise_before_sound) - taper_samples + 1):length(noise_before_sound)] <- 
    noise_before_sound[(length(noise_before_sound) - taper_samples + 1):length(noise_before_sound)] * rev(seq(0, 1, length.out = taper_samples))
  
  # Apply fade-in to the beginning of noise_after_sound
  noise_after_sound[1:taper_samples] <- noise_after_sound[1:taper_samples] * seq(0, 1, length.out = taper_samples)
  
  # Concatenate noise_before_sound, triplet, and noise_after_sound
  result <- c(noise_before_sound, triplet, noise_after_sound)
  
  # Save the result to a WAV file
  savewav(result, filename = paste(dir, triplet.name, sep = ''), extensible = FALSE, f = sample.rate)
  
  return(result)
}
##########################################################
##########################################################


# Function to apply a lowpass filter to the noise
filter_signal <- function(signal, sample.rate, frequencies) {
  # Specify the filter order 
  filter_order <- 4  # Adjust this value as needed
  
  # Normalize frequencies to Nyquist frequency
  nyquist <- 0.5 * sample.rate
  normalized_freqs <- c(min(frequencies), max(frequencies)) / nyquist
  
  # Create a bandpass filter using the 'signal' package's butter function
  filter_object <- signal::butter(n = filter_order, type = "pass", W = normalized_freqs)
  
  # Apply the filter to the signal
  filtered_signal <- signal::filter(filter_object, signal)
  
  return(filtered_signal)
}
##########################################################
##########################################################


generate_filtered_noise <- function(samples, sample.rate, freq1, freq2, freq3, amplitude) {
  
  # Generate white noise
  white_noise <- rnorm(samples, mean = 0, sd = 0.2 * amplitude)
  
  # Filter the white noise to match the frequency content of the signals
  # NOTE: adjust frequency range desired for the white noise
  filtered_noise <- filter_signal(white_noise, sample.rate, c(200, 7000))
  
  return(filtered_noise)
}
##########################################################
##########################################################


merge.wav <- function(directory, output.name) {
  library('phonfieldwork')
  file.names <- list.files(path = directory, pattern = '.wav')
  concatenate_soundfiles(directory,
                         result_file_name = output.name,
                         annotation = NULL)
  
}
# all.signals <- appendSample(paste0(directory,file.names))
### Sound package may have a alternative solution to merge wav files. "appendsample" function

