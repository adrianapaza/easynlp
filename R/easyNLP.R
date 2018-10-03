# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(tm)
##library(roxygen2)execute this
library(text2vec)
library(data.table)
library(gsubfn)
library(stringr)
#' Removes Numbers and Punctuation characters
#'
#' This function removes punctuation and numbers
#' @param string An input that may be a string or dataframe column
#' @return returns the input in same format but without punctuation or numbers
#' @export
#' @examples
#' Remove_Bad_Chars('1 plus 1.1 = 2.1')
Remove_Bad_Chars<-function(string){
  x <- as.character(string)
  x <- gsubfn(".", list("'" = "", "-" = "", "_" = " ", "," = "","<" = "",">" = ""), x)
  x <- str_replace_all(x, "[[:punct:]]", "")
  x <-gsub('[0-9]+', '',x)

  return(x)
}




#' Gets words of certain vocaublary from an input soruce (String, column, etc)
#'   which you may wish to remove later on.
#' @param RemovableData An input that may be a string or dataframe column
#' @return Returns all the individual words that you do not want
#' @export
#' @examples
#' Vocab_to_Remove(c('one direction','kale','russia'))
Vocab_to_Remove<-function(RemovableData){
  #get names of words to remove
  names <-RemovableData
  names <- gsub(",", ' ', names)
  prep_fun = tolower
  tok_fun = word_tokenizer
  names_all = itoken(names,
                     preprocessor = prep_fun,
                     tokenizer = tok_fun,
                     #ids = allDataset$all_ids,
                     progressbar = FALSE)
  #Get a frequency of each term into vocab vector
  namesAll = create_vocabulary(names_all)
  vectornames<-as.vector(namesAll)
  vectornames<-vectornames[,1]
  stop_words<-c(vectornames)
  return(stop_words)
}


#' Input a dataframe of corpus and output document term Matrix and vocab
#'   specify column with text to use
#' @param input A dataframe of a text corpus
#' @param col The column of the dataframe you wish to use; defaults to one
#' @param words_to_remove The words you wish to not be included from Vocab_to_Remove function.
#' defaults to NULL
#' @param return_embedding_params Whether to to include the itokens and vocab_vectorizer in the output
#' which is needed to pass to train_embeddings if you wish to train embeddings on your own data.
#' Defaults to false.
#' @return Returns a list of the document term matrix and vocabulary with term counts.
#' @export
#' @examples
#' Create_Vocab_Document_Term_Matrix(MyData,col=1,words_to_remove=c('kale','lump'))
Create_Vocab_Document_Term_Matrix<-function(input,col=1,words_to_remove=NULL,return_embedding_params=F){
  DF<-as.data.frame(input)
  ##need to give each row an ID
  DF$ID <- seq.int(nrow(DF))
  DFMat<- as.matrix(DF[,col],DF$ID)
  ##Prepare for tokenization
  setDT(DF)
  setkey(DF, ID)
  all_ids = DF$ID
  allDataset<-DF[J(all_ids)]
  ?J
  colnames(allDataset)<-c('text','all_ids')
  #Now we wish to tokenize our input strings of words by implementing text2vec in R
  prep_fun = tolower
  tok_fun = word_tokenizer
  it_all = itoken(as.character(allDataset$text),
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = allDataset$all_ids,
                  progressbar = FALSE)
  #Get a frequency of each term into vocab vector

  if(!(is.null(words_to_remove))){
    vocabAll = create_vocabulary(it_all,stopwords = words_to_remove)
  }
  else {
    vocabAll = create_vocabulary(it_all)
    ##create document term matrix
  }
  vectorizeall=vocab_vectorizer(vocabAll)
  ?vocab_vectorizer
  dtm_All = create_dtm(it_all, vectorizeall)

  if (return_embedding_params == T){
    return(list("DTM"=dtm_All,"vocab"=vocabAll,"itokens"=it_all,"vocab_vectorizer"=vectorizeall))
  }

  else {
    return(list("DTM"=dtm_All,"vocab"=vocabAll))
  }
}


#' Trains embeddings from your corpus using methods described here: http://nlp.stanford.edu/pubs/glove.pdf.
#' Using the text2vec package; see that package for more info.
#' @param input A dataframe of a text corpus
#' @param it_all The tokens from Create_Vocab_Document_Term_Matrix.
#' @param vocab_vectorizer The vocabulary vectorizer from Create_Vocab_Document_Term_Matrix
#' @param window The window size for word co-occurences
#' @param dimensions The number of dimensions returned for word embeddings. Defaults to 100
#' @param max_iters The maximum number of iterations for training the embeddings. Defaults to 50
#' @param max_cooccur The maximum number of times a word-word co-occurence may be used in weighting
#' the model. Defaults to 50. Value should be proportional to amount of data.
#' @return Returns a dataframe of word embeddings
#' @export
#' @examples
#' train_embeddings(Myvocab, itokens, vocab_vectorizer, window=10, dimensions=100, max_iters=50, max_cooccur=50)
train_embeddings<-function(vocab, it_all, vocab_vectorizer,
                            window=10, dimensions=100, max_iters=50, max_cooccur=50){
  ###Now let's get a term co-occurence matrix, (this gets's all terms withn N L terms of a given term in each description)
  #we use 'symmetric' to get terms both preceding and after the given term
  tcm <- create_tcm(it_all, vocab_vectorizer, skip_grams_window = window,
                    skip_grams_window_context = 'symmetric');
#now use glove trained on our corpus of descriptions. and use both 100 dimensions and 200
  ##see page 4 of glvoe paper for more info on this
glove = GlobalVectors$new(word_vectors_size = dimensions, vocabulary = vocab, x_max = max_cooccur);
wv_main = glove$fit_transform(tcm, n_iter = max_iters, convergence_tol = 0.001);

wv_context = glove$components;
word_vectors = wv_main + t(wv_context);
GloveDataFrame<-as.data.frame.matrix(word_vectors);
##order it alphabetically
GloveDataFrame<-GloveDataFrame[order(rownames(GloveDataFrame)),]
return(GloveDataFrame)
}



##create document term matrix
#vectorizerAll = vocab_vectorizer(pruned_vocab)






#' Create a matrix of embeddings
#' @param DocTermMatrix The document term matrix
#' @param Embeddings The matrix of embeddings (e.g. Glove) where rows are words
#' @param Vocabulary The Vocab list or dataframe from Create_Vocab_DocumentTermMatrix
#' @param LogInverse whether to weight words using LogInverse; defaults to FALSE
#' @return Returns the average embedding of each document for each embedding dimension.
#' @export
#' @examples
#' CreateEmbeddingMatrix(MyDocTermMatrix, Glove.200, Vocab, LogInverse=T)
CreateEmbeddingMatrix <- function(DocTermMatrix,Embeddings,Vocabulary,LogInverse) {
  ##Turn the matrix of all the words into a dataframe.
  DTMALL<-as.data.frame.matrix(DocTermMatrix)
  #order it alphabetically
  DTMALL<-DTMALL[,order(colnames(DTMALL))]
  #keep only words in both the trained words and the dataframe
  keep <- colnames(DTMALL)
  Embeddings<-Embeddings[rownames(Embeddings) %in% keep, ]
  keep<-rownames(Embeddings)
  DTMALLOVerlap300<-DTMALL[,colnames(DTMALL) %in% keep ]


  #make sure they are ordered alphabetically so that the words overlap
  EmbeddingsDataFrame<-Embeddings[order(rownames(Embeddings)),]
  #order the other dataframe  alphabetically
  DTMALLOVerlap300<-DTMALLOVerlap300[,order(colnames(DTMALLOVerlap300))]

  #Get the average word use per document
  DTMALLOverlapMeans300<-as.matrix(DTMALLOVerlap300)/rowSums(DTMALLOVerlap300)

  if (LogInverse==T) {
    ##get voacabulary pruned
    vocabAllPruned<-Vocabulary[Vocabulary$term %in% keep,]
    vocabAllPruned<-vocabAllPruned[order(vocabAllPruned$term),]
    ##get log inverse count
    nn=nrow(DTMALLOVerlap300)
    ##get weighting of words by inverse frequency
    inverseLogDocumentCount<-log(nn/ vocabAllPruned$doc_count)

    ##get the term glove matrix weighted by inverse log frequency
    InverseLogDocFreqMatrix<-as.matrix(inverseLogDocumentCount) ##this is a column matrix
    DTMInverseWeightedMeans<-apply(DTMALLOverlapMeans300, 1, function(.DTMALLOverlapMeans300)mapply(InverseLogDocFreqMatrix, .DTMALLOverlapMeans300, FUN="*"))
    MeanOverlap<- as.matrix(t(DTMInverseWeightedMeans))%*%as.matrix(Embeddings)
    return(as.data.frame(MeanOverlap))
  }
  if (LogInverse==F) {

    ##Now multiply the matrices
    MeanOverlap<- as.matrix(DTMALLOverlapMeans300)%*%as.matrix(Embeddings)
    #Get the final output as a data frame

    ##multiply
    AverageVectorOverlapDF300<-as.data.frame(MeanOverlap)
    return(AverageVectorOverlapDF300)
  }
}






#' Remove common gender pronouns and words from a corpus
#' @param x The input of strings(datframe,vector,etc)
#' @param remove_familial Removes familial gender terms: aunt, uncle, mom, dad,
#'  mother, father, husband, wife, son, daughter, niece, nephew, grandmother,
#'  grandfather. Defaults to true
#' @return Returns the input but without common gendered words
#' @export
#' @examples
#' remove_gendered_words(MyCorpus)
remove_gendered_words<-function(x,remove_familial=T) {
  #Drop gendered words
  x <- gsub("\\<Her\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<His\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<She\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Shes\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Ms\\>", ' ', x,,ignore.case = T)
  x <- gsub("\\<Miss\\>", ' ', x,,ignore.case = T)
  x <- gsub("\\<Mrs\\>", ' ', x,,ignore.case = T)
  x <- gsub("\\<Mr\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<He\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Hes\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Him\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Men\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Man\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Women\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Male\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Female\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<Woman\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<himself\\>", ' ', x,ignore.case = T)
  x <- gsub("\\<herself\\>", ' ', x,ignore.case = T)
  if(remove_familial == T) {
    x <- gsub("\\<Mother\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<Father\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<Mom\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<dad\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<Husband\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<Wife\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<son\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<daughter\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<uncle\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<aunt\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<niece\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<nephew\\>", ' ', x,ignore.case = T)
    x <- gsub("\\<grandmother\\>", ' ', x,ignore.case = T)
  }
  return(x)
}


#' Does a search for the best neural net to fit the data using a restricted h2o.automl function.
#' See this function in h2o documentation for more info.
#' You will need the h2o package. And you may have to download an older version of java for it to work.
#' @param Embeddings The embedding dataframe or matrix
#' @param dependent_var The variable used to predict, must be a factor
#' @param max_minutes The maximum run time for model search if stopping tolerance not reached.
#' For larger data sets you may need multiple hours. Defaults to 10
#' @param stopping_tol The stopping tolerance for model search. Defaults to .005
#' @param nfolds The number of folds for k-fold cross-validation. Defaults to 5
#' @return Returns the best model
#' @export
#' @examples
#'
find_best_neural_net<-function(Embeddings, dependent_var, max_minutes=10, stopping_tol=0.005,n_folds=5){
  library(h2o)
  h2o.init()
  NeuralData<-cbind(Embeddings,dependent_var)
  NeuralData.h2o <- as.h2o(NeuralData)
  results<-h2o.automl(y=ncol(NeuralData), training_frame =  NeuralData.h2o, validation_frame = NULL,
                  leaderboard_frame = NULL, nfolds = n_folds, fold_column = NULL,
                  weights_column = NULL, balance_classes = FALSE,
                  class_sampling_factors = NULL, max_after_balance_size = 5,
                  max_runtime_secs = max_minutes*60, max_models = NULL,
                  stopping_metric = "logloss", stopping_tolerance = stopping_tol,
                  stopping_rounds = 3, seed = NULL, project_name = NULL,
                  exclude_algos =  c("GLM", "GBM", "DRF","StackedEnsemble"),
                  keep_cross_validation_predictions = TRUE,
                  keep_cross_validation_models = TRUE,
                  sort_metric = "logloss")
  return(results@leader)
}
