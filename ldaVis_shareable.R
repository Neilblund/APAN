LDAvis_standalone <- function(model, outputfile) {
  require(LDAvis)
  # method for WarpLDA
  if(class(model)[1] == "WarpLDA"){
    json = createJSON(
      phi = model$.__enclos_env__$private$topic_word_distribution_with_prior(),
      theta = model$.__enclos_env__$private$doc_topic_distribution_with_prior(),
      doc.length = model$.__enclos_env__$private$doc_len,
      vocab = model$.__enclos_env__$private$vocabulary,
      term.frequency = colSums(model$.__enclos_env__$self$components),
      reorder.topics = FALSE
      
    )
  }
  # method for textmodel_lda
  if(class(model)[1] == 'textmodel_lda'){
    json<-createJSON(
      phi = model$phi,
      theta = model$theta,
      doc.length = rowSums(model$data),
      vocab = colnames(model$data),
      term.frequency = colSums(model$data),
      reorder.topics = FALSE)
  }
  
  if(class(model)[1] == "keyATM_output"){
    json = createJSON(
      phi = model$phi,
      theta = model$theta,
      doc.length = model$doc_lens, 
      vocab = model$vocab,
      term.frequency = model$word_counts,
      reorder.topics = FALSE
    )
    
  }
  
  
  id_name<-paste0('eldavis_', gsub("[^0-9]", "", as.numeric(Sys.time())), collapse="")
  
  
  cat(
    sprintf(
      '<link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/gh/bmabey/pyLDAvis@3.4.0/pyLDAvis/js/ldavis.v1.0.0.css">

<div id="%s" style="background-color:white;"></div>
    <script type="text/javascript">
    var ldavis_data = %s

function LDAvis_load_lib(url, callback){
  var s = document.createElement(\'script\');
  s.src = url;
  s.async = true;
  s.onreadystatechange = s.onload = callback;
  s.onerror = function(){console.warn("failed to load library " + url);};
  document.getElementsByTagName("head")[0].appendChild(s);
}

if(typeof(LDAvis) !== "undefined"){
   // already loaded: just create the visualization
   !function(LDAvis){
       new LDAvis("#" + "%s", ldavis_data);
   }(LDAvis);
}else if(typeof define === "function" && define.amd){
   // require.js is available: use it to load d3/LDAvis
   require.config({paths: {d3: "https://d3js.org/d3.v5"}});
   require(["d3"], function(d3){
      window.d3 = d3;
      LDAvis_load_lib("https://cdn.jsdelivr.net/gh/bmabey/pyLDAvis@3.4.0/pyLDAvis/js/ldavis.v3.0.0.js", function(){
        new LDAvis("#" + "%s", ldavis_data);
      });
    });
}else{
    // require.js not available: dynamically load d3 & LDAvis
    LDAvis_load_lib("https://d3js.org/d3.v5.js", function(){
         LDAvis_load_lib("https://cdn.jsdelivr.net/gh/bmabey/pyLDAvis@3.4.0/pyLDAvis/js/ldavis.v3.0.0.js", function(){
                 new LDAvis("#" + "%s", ldavis_data);
            })
         });
}
</script>', id_name ,paste(json), id_name, id_name, id_name), file  =  outputfile)}
