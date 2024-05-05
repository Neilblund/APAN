stem_unstem<-function(dfm, matchby='frequency'){
  require(quanteda)
  if(matchby == 'shortest'){
    vocab<-colnames(dfm)[order(nchar(colnames(dfm)), decreasing=T)]
  }
  if(matchby == 'frequency'){
    vocab<-names(sort(featfreq(dfm), decreasing=T))}
  stemmed<-char_wordstem(vocab, check_whitespace=FALSE)
  dfm<-dfm_replace(dfm, vocab, stemmed)
 
  dfm<- dfm_replace(dfm, featnames(dfm), vocab[match(featnames(dfm), stemmed)])
  return(dfm)
}
