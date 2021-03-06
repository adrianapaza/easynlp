% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyNLP.R
\name{train_embeddings}
\alias{train_embeddings}
\title{Trains embeddings from your corpus using methods described here: http://nlp.stanford.edu/pubs/glove.pdf.
Using the text2vec package; see that package for more info.}
\usage{
train_embeddings(vocab, it_all, vocab_vectorizer, window = 10,
  dimensions = 100, max_iters = 50, max_cooccur = 50)
}
\arguments{
\item{it_all}{The tokens from Create_Vocab_Document_Term_Matrix.}

\item{vocab_vectorizer}{The vocabulary vectorizer from Create_Vocab_Document_Term_Matrix}

\item{window}{The window size for word co-occurences}

\item{dimensions}{The number of dimensions returned for word embeddings. Defaults to 100}

\item{max_iters}{The maximum number of iterations for training the embeddings. Defaults to 50}

\item{max_cooccur}{The maximum number of times a word-word co-occurence may be used in weighting
the model. Defaults to 50. Value should be proportional to amount of data.}

\item{input}{A dataframe of a text corpus}
}
\value{
Returns a dataframe of word embeddings
}
\description{
Trains embeddings from your corpus using methods described here: http://nlp.stanford.edu/pubs/glove.pdf.
Using the text2vec package; see that package for more info.
}
\examples{
train_embeddings(Myvocab, itokens, vocab_vectorizer, window=10, dimensions=100, max_iters=50, max_cooccur=50)
}
