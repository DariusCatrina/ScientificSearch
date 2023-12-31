---  
title: Creating Odinson Documents  
parent: Getting Started
has_children: false  
nav_order: 2 
---  

# Annotating text  
Before an Odinson index can be created, the text needs to be annotated.  You may use your **own annotation tools**, as long as you convert your annotated output to [Odinson Documents](https://github.com/lum-ai/odinson/blob/master/core/src/main/scala/ai/lum/odinson/OdinsonDocument.scala).  
  
However, we also provide an App for annotating free text and producing this format, which makes use of the [clulab Processors library](https://github.com/clulab/processors).  
  
### Configuration  
The configurations are specified in `extra/src/main/resources/application.conf`.  
  
- First, decide what Processor you'd like to use to annotate the text by specifying a value for `odinson.extra.processorType`.  Available options are `FastNLPProcessor`, and `CluProcessor`. For more information about these, see [clulab Processors](https://github.com/clulab/processors).  
  
- Ensure `odinson.textDir` and `odinson.docDir` are set as intended.  Text will be read from `odinson.textDir`, annotated, and serialized to `odinson.docDir`.    
**NOTE**: We recommend a directory structure where you will have a data folder with subdirs `text`, `docs`, and `index`.  If you do this, you can simply specify `odinson.dataDir = path/to/your/dataDir`, and the subfolders will be handled.  
  
  
### Memory Usage  
  
Depending on the number and size of the documents you are annotating, this step can be memory intensive.  We recommend you set aside at least 8g, but if you have more it will run faster. You can specify this through this command:  

	 export SBT_OPTS="-Xmx8g"  
 
## Command  
  
	sbt "extra/runMain ai.lum.odinson.extra.AnnotateText"  
 
This step may take time, highly dependent on the length of your documents and the size of your corpus.  
