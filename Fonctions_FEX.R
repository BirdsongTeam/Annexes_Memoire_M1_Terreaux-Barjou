##############################################################################################
# FONCTIONS POUR L ANALYSE DE COMPLEXITE VOCALE
##############################################################################################


##############################################################################################
# IMPORT.CUT
# importer les enregistrements et les couper selon les sélections
# sel.table = la grande table de selection qui a été importée avec imp_raven de Rraven
# dossier = où stocker les fichiers découpés
# sort une liste avec les objets wave découpés

import.cut <- function(sel.table = NULL, dossier = NULL) {
  total_cut <- c()
  names_cut_recordings <- c()
  dossier_cut <- dossier
  
  for (i in 1:length(sel.table$sound.files)) {
    #name current file
    nom <- sel.table$sound.files[i]
    #path current file
    file_name <- paste(raw, "/", nom, sep="")
    
    #name for write wave
    nom_sans_wav <- file_path_sans_ext(nom)
    num_selec <- sel.table$selec[i]
    cut_name <- paste(dossier_cut, "/", nom_sans_wav, "_", num_selec, ".wav", sep="")
    
    #name for R storage
    R_name <- paste(nom_sans_wav, "_", num_selec, sep="")
    names_cut_recordings <- c(names_cut_recordings, R_name)
    
    #temps
    t_init <- sel.table$start[i]
    t_fin <- sel.table$end[i]
    
    #importation
    current_cut <- readWave(file_name, from=t_init, to=t_fin, units="seconds")
    #R storage
    total_cut <- c(total_cut, current_cut)
    #write wave
    writeWave(object = current_cut, filename = cut_name, extensible = FALSE)
    #progress
    print(c(i, cut_name))
   # spectro(current_cut)
    
  }
  #R storage
  names(total_cut) <- names_cut_recordings
  return(total_cut)
}
##############################################################################################



###############################################################################################
# FILTRATION
# filtrer les morceaux coupés par rapport aux bornes de frq sup et inf des selections
# cut.recordings = la sortie de la fonction import.cut
# dossier = où stocker les fichiers filtrés
# sort une liste avec les objets wave filtrés

filtration <- function(cut.recordings = NULL, dossier = NULL, sel.table = NULL) {
  total_filtered <- c()
  dossier_filtre <- dossier
  names_filtered_recordings <- c()
  
  for (i in 1:length(cut.recordings)) {
    #name of current file
    nom <- names(cut.recordings)[[i]]
    #name for R storage
    new_name <- paste(nom, "_filtered", sep="")
    names_filtered_recordings <- c(names_filtered_recordings, new_name)
    #name for write wave
    filtered_path <- paste(dossier_filtre, "/", new_name, ".wav", sep="")
    
    #frequences pour le filtre
    bottom_freq <- sel.table$bottom.freq[sel.table$cut.name==nom]
    bottom_freq <- bottom_freq*1000 #pour convertir Hz en kHz
    top_freq <- sel.table$top.freq[sel.table$cut.name==nom]
    top_freq <- top_freq*1000 #pour convertir Hz en kHz
    
    #Band pass filter
    dft <- ffilter(cut.recordings[[i]], f=44100, from = bottom_freq, to = top_freq, output="Wave")
    spectro(dft)
    #R storage
    total_filtered <- c(total_filtered, dft)
    #write wave
    writeWave(object = dft, filename = filtered_path, extensible = FALSE)
    #progress
    print(c(i, new_name))
    
  }
  #R storage
  names(total_filtered) <- names_filtered_recordings
  return(total_filtered)
  
}
##############################################################################################



###############################################################################################
# CEILING_DEC
# arrondir un nombre au supérieur
# x = nombre à arrondir
# level = combien de décimales

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
##############################################################################################



###############################################################################################
# FLOOR_DEC
# arrondir un nombre à l'inférieur
# x = nombre à arrondir
# level = combien de décimales

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
##############################################################################################



###############################################################################################
# FREQUENCY.EXCURSION
# calculer l'indice de frequency excursion pour un fichier audio (Podos et al, 2016)
# wav_file = le fichier audio préalablement importé et filtré
# overlap, wl = paramètres de spectrogramme
# window_size = time bin for peak frequency calculation, in seconds, by default window_size = 0.0058, meaning that peak frequency will be calculated every 5.8 milliseconds.
# freq_interval = intervalle de fréquence dans lequel se situe le signal
# dBlevel = seuil pour le bruit de fond
# log = pour utiliser une echelle log pour les fréquences
# sort la valeur de l'indice

frequency.excursion <- function(wav_file, overlap=70, wl = 512, window_size = 0.0058, freq_interval = c(0, 25),  dBlevel=25, log=TRUE) {
  result <- list()
  
  # Recover the sampling rate of your WAV file, for calculations
  sr <- wav_file@samp.rate
  
  # 2. Calculate and plot spectrogram. We're using greyscale which will make the contour points more visible
  if(log==TRUE) {
    spec_result <- spectro(wav_file, f = sr, wl = wl, ovlp = overlap, flog = TRUE, plot=F)
  }
  if(log==FALSE) {
    spec_result <- spectro(wav_file, f = sr, wl = wl, ovlp = overlap, plot=F)
  }
  
  # 3. Calculate peak frequency every 5.8 milliseconds with 75% overlap
  window_size_samples <- round(window_size * sr)  # 5.8ms in samples
  hop_size <- round(window_size_samples / 4)  # 75% overlap (1.45ms hop)
  
  # Calculate the number of windows for the contour plot
  n_samples <- length(wav_file@left)
  n_windows <- floor((n_samples - window_size_samples) / hop_size) + 1 #nombre de fenetres chevauchantes à 75% de taille voulue
  
  # Initialize vectors for time, peak frequencies, and amplitudes
  time_points <- seq(0, (n_windows - 1) * hop_size / sr, by = hop_size / sr)
  peak_freqs <- numeric(n_windows)
  window_amplitudes <- numeric(n_windows)
  
  # Calculate peak frequency for each window, in several steps
  for (i in 1:n_windows) {
    start_sample <- (i - 1) * hop_size + 1
    end_sample <- min(start_sample + window_size_samples - 1, n_samples)
    
    # Extract window
    window_data <- wav_file@left[start_sample:end_sample]
    
    # Calculate spectrum and find peak frequency
    if (length(window_data) > 1) {
      # Use meanspec to get frequency spectrum
      spectrum <- meanspec(window_data, f = sr, plot = FALSE, wl = min(wl, length(window_data)))

      # Find peak frequency
      peak_idx <- which.max(spectrum[,2])  # spectrum[,2] contains amplitude values
      if (log==TRUE) {
        peak_freq_Hz <- spectrum[peak_idx, 1]  # spectrum[,1] contains frequency values in kHz, keep in kHz
        peak_freqs[i] <- log(peak_freq_Hz+1) #transformation log [ln], mais vérifier les infinis
      }
      if (log==FALSE) {
        peak_freqs[i] <- spectrum[peak_idx, 1] # spectrum[,1] contains frequency values in kHz, keep in kHz
      }
      
      # Calculate RMS amplitude for this window (in dB)
      window_amplitudes[i] <- 20 * log10(sqrt(mean(window_data^2)) + 1e-10)  # Add small value to avoid log(0)
    } else {
      peak_freqs[i] <- NA
      window_amplitudes[i] <- NA
    }
  }
  
  # 4. Calculate peak frequency contours
  if(log==TRUE) {
    spectro(wav_file, f = sr, wl = wl, ovlp = overlap,
            main = "Spectrogram with Peak Frequency Points", flog=TRUE)
  }
  if (log==FALSE) {
    spectro(wav_file, f = sr, wl = wl, ovlp = overlap,
            main = "Spectrogram with Peak Frequency Points")
  }
  
  # Filter out points that are softer than -24 dB relative to the maximum amplitude in the file
  max_amplitude_db <- max(window_amplitudes, na.rm = TRUE)
  amplitude_threshold <- max_amplitude_db - dBlevel  # 24 dB softer than loudest
  
  
  # Filter out points below 500 Hz (0.5 kHz) and above 10000 Hz (10 kHz) -- this can be tweaked too if needed
  valid_indices <- which(window_amplitudes >= amplitude_threshold)  # Frequency and amplitude filtering
  filtered_time_points <- time_points[valid_indices]
  filtered_peak_freqs <- peak_freqs[valid_indices]
  filtered_amplitudes <- window_amplitudes[valid_indices]
  
  # Add remaining peak frequency points as dots
  points(filtered_time_points, filtered_peak_freqs, col = "white", pch = 19, cex = 1.2)
  points(filtered_time_points, filtered_peak_freqs, col = "black", pch = 19, cex = 0.8)
  
  # Add line segments connecting all sequential filtered points
  if (length(filtered_peak_freqs) >= 2) {
    lines(filtered_time_points, filtered_peak_freqs, col = "black", lwd = 1)
  }
  
  # Add legend
  legend("topright", legend = paste("Peak Frequency (", freq_interval[1], ",", freq_interval[2], "kHz, ≥-", dBlevel, "dB)"),
         col = "black", pch = 19, bg = "white")
  
  # Calculate Frequency Excursion Index (Podos et al. 2016)
  
  if (length(filtered_peak_freqs) >= 2) {
    # Step 1: Calculate linear distances between all adjacent peak frequency points
    # Using Euclidean distance in time-frequency space
    distances <- numeric(length(filtered_peak_freqs) - 1)
    
    for (i in 1:(length(filtered_peak_freqs) - 1)) {
      # Time difference (in seconds)
      time_diff <- filtered_time_points[i + 1] - filtered_time_points[i]
      # Frequency difference (in kHz)
      freq_diff <- filtered_peak_freqs[i + 1] - filtered_peak_freqs[i]
      # Linear distance in time-frequency space
      distances[i] <- sqrt(time_diff^2 + freq_diff^2)
    }
    
    # Step 2: Sum all linear distances
    total_distance <- sum(distances)
    
    # Step 3: Calculate total duration from first to last peak frequency point
    total_duration <- max(filtered_time_points) - min(filtered_time_points)
    
    # Step 4: Calculate Frequency Excursion Index
    frequency_excursion_index <- total_distance / total_duration
    result <- c(frequency_excursion_index)
    
    # Print results
    cat("\n=== FREQUENCY EXCURSION INDEX (Podos et al. 2016) ===\n")
    #cat("Total linear distance:", round(total_distance, 4), "units\n")
    #cat("Total duration:", round(total_duration, 4), "seconds\n")
    cat("Frequency Excursion Index:", round(frequency_excursion_index, 4), "units/second\n")
    #cat("Number of peak frequency points used:", length(filtered_peak_freqs), "\n")
    #cat("Number of segments calculated:", length(distances), "\n")
    #cat("Maximum amplitude:", round(max_amplitude_db, 2), "dB\n")
  } else {
    cat("\n=== FREQUENCY EXCURSION INDEX ===\n")
    cat("Insufficient data points (need at least 2) to calculate frequency excursion index.\n")
  }
  return(result)
}
###############################################################################################



###############################################################################################
# FEX.R
# calculer l'indice de FEX pour tous les audios d'une liste (type sortie de la fonction filtration)
# data = liste avec les fichiers filtrés (sortie de filtration)
# sel.table = la grande table de sélection importée avec rraven
# overlap, wl = paramètres de spectrogramme
# window_size = time bin for peak frequency calculation, in seconds, by default window_size = 0.0058, meaning that peak frequency will be calculated every 5.8 milliseconds.
# dBlevel = seuil pour le bruit de fond
# log = pour utiliser une echelle log pour les fréquences
# sort une liste avec les valeurs d'indice pour chaque audio de la liste

fex.r <- function(data = NULL, sel.table = NULL, overlap = 20, wl = 256, window_size = 0.0058, dBlevel = 25, log = TRUE) {
  total_fex <- c()
  names_fex <- c()
  
  for (i in 1:length(data)) {
    #name of current file
    nom <- names(data)[i]
    nom <- substr(nom,1,nchar(nom)-9)
    #name for R storage
    names_fex <- c(names_fex, nom)
    
    #calcul fex
    fex_current <- frequency.excursion(data[[i]], overlap = overlap, wl = wl, window_size = window_size, freq_interval = c(0,25), dBlevel = dBlevel, log = log) #ici pas de limite sur l'intervalle de fréquences vu que les signaux ont déjà été BPF
    total_fex <- c(total_fex, fex_current)
    
    print(c(i, nom))
    
  }
  names(total_fex) <- names_fex
  return(total_fex)
}
###############################################################################################


