# Function for a set of pairwise alignments of sequences in two dataframes.
# Dataframes need columns named "sequence" and "uniparc_id"

multi_pairwise_aln <- function(pattern_df, subject_df, pid_alg = "PID1", sub_matrix = "BLOSUM62") {

  # Check if valid substition matrix.
  if (!sub_matrix %in% c("BLOSUM45", "BLOSUM50", "BLOSUM62", "BLOSUM80", "BLOSUM100")) {
    return(cat("Invalid substitution matrix!"))
  }

  # Check if valid %ID algorithm.
  if (!pid_alg %in% c("PID1", "PID2", "PID3", "PID4")) {
    return(cat("Invalid %ID algorithm!"))
  }
  
  # Grab names of dataframes (for use in naming the .csv )
  pattern_df_name  <- deparse(substitute(pattern_df))
  subj_df_name     <- deparse(substitute(subject_df))
  
  # Named character vector of all pattern sequences
  pattern_seqvec <- pattern_df$sequence
  names(pattern_seqvec) <- pattern_df$uniparc_id
  
  # Named character vector of subject sequences
  subject_vec <- subject_df$sequence
  names(subject_vec) <- subject_df$uniparc_id
  
  # Convert vectors of sequences to AAStringSets; also create vector of names for later.
  pattern_seq <- AAStringSet(pattern_seqvec)
  subject_seq <- AAStringSet(subject_vec)
  pattern_names <- pattern_df$uniparc_id
  subject_names <- subject_df$uniparc_id
  
  
  # Perform pairwise alignments for each pattern sequence with each subject sequence.
  # Also compute percent identity, store in separate list.
  alnlist <- list()
  pidlist <- list()
  for (i in seq_along(subject_seq)) {
    alnlist[[i]] <- pairwiseAlignment(pattern_seq, subject_seq[i], 
                                      substitutionMatrix = sub_matrix)
    pidlist[[i]] <- pid(alnlist[[i]], type = pid_alg)
    names(pidlist[[i]]) <- pattern_names
  }
  
  # Generate dataframe of pattern alignments; also include percent ID.
  alnlist_pattern <- list()
  alnlist_subject <- list()
  for (i in seq_along(alnlist)) {
    alnlist_pattern[[i]] <- alnlist[[i]] %>% 
      pattern() %>% 
      as.character() %>% 
      enframe() %>%
      mutate(pattern_uniparc = pattern_names, pattern_aln = value, pid = pidlist[[i]]) %>% 
      select(pattern_uniparc, pattern_aln, pid)
  }
  
  # Generate dataframe of subject (i.e. aligned-to sequence) alignments.
  for (i in seq_along(alnlist)) {
    alnlist_subject[[i]] <- alnlist[[i]] %>% 
      subject() %>% 
      as.character() %>% 
      enframe() %>% 
      mutate(pattern_uniparc = pattern_names, subject_aln = value) %>% 
      select(pattern_uniparc, subject_aln)
  }
  
  # Name elements of the pattern/subject alignment lists.
  names(alnlist_pattern) <- subject_names
  names(alnlist_subject) <- subject_names
  
  # Combine the pattern/subject lists to create a single (long) dataframe.
  alndf_pattern <- alnlist_pattern %>% 
    enframe() %>% 
    unnest() %>% 
    mutate(subject_uniparc = name) %>% 
    select(-name)
  
  alndf_subject <- alnlist_subject %>% 
    enframe() %>% 
    unnest() %>% 
    mutate(subject_uniparc = name) %>% 
    select(-name)
  
  # Create the full dataframe!
  full_alndf <- alndf_pattern %>% 
    left_join(alndf_subject)
  
  # Summarize the full dataframe by highest %ID alignment for each sequence
  alndf_summary <- full_alndf %>% 
    filter(pid != 100) %>% 
    group_by(pattern_uniparc) %>% 
    filter(pid == max(pid)) %>% 
    distinct(pattern_uniparc, .keep_all = TRUE)
  
  # Write the .csv files!
    write_csv(full_alndf,
              glue("pairwise-distmat/pairwisealn_{subj_df_name}_{sub_matrix}_{pid_alg}.csv"))
    write_csv(alndf_summary,
              glue("pairwise-distmat/pairwisealn_summary_{subj_df_name}_{sub_matrix}_{pid_alg}.csv"))

}
