file_name <- "/Users/Mac/code/codeFreq/poetryFromTang.txt"

library(RColorBrewer)
library(wordcloud2)
library("jiebaRD")
library("jiebaR")


#analyze the plain text
analyze<-function(fileName = file_name){
    text<-readChar(fileName,file.info(fileName)$size);
    
    segWorker = worker()

    analysis<-as.data.frame(table(factor(segment(text,segWorker))))

    names(analysis)<-c("word","freq")
    analysis<-analysis[order(-analysis$freq),]
    analysis$word<-as.character(analysis$word);
    head(analysis);
    wordcloud2(analysis[analysis$freq > 5 & nchar(analysis$word) > 1, ]);
}

#analyze the text which the structure as below:
#詩名:杜少府之任蜀州
#作者:王勃
#詩體:五言律詩
#詩文:(押真韻)城闕輔三秦，風煙望五津。與君離別意，同是宦遊人。海內存知己，天涯若比鄰。無為在歧路，兒女共沾巾。
analyzeStructure<-function(fileName = "/Users/Mac/data/corpus/唐詩三百首.txt"){
    text<-readChar(fileName,file.info(fileName)$size);
    #Erase the Chinese Characters wrapped with "()".
    #TODO: improve the regex.
    pattern <- "[(]\\w*[\u4e00-\u9fa5]*\\w*[)]";
    text<-gsub(pattern,"",text);
    lines = unlist(strsplit(text,"\n"));
    pattern = "[\u4e00-\u9fa5]+:[\u4e00-\u9fa5]+";
    #Erase the useless text.
    lines = lines[grepl(pattern,lines)];
    #需对title,content进行分词，author和content作为MetaData
    title = "詩名";
    titleText = "";

    author = "作者";
    authorVector = c();
    
    style = "詩體";
    styleVector = c();
    
    content = "詩文";
    contentText = "";

    for(i in 1:length(lines)){
        segments = unlist(strsplit(lines[i],":"));
        if(length(segments) == 2){
            key = segments[1];
            value = segments[2];
            #print(key);
            #print(title);
            if (key == title) {
                titleText = paste(titleText,value);
            }else {
                if(key == author){
                    authorVector = c(authorVector,value);
                }else {
                    if(key == content) {
                        contentText = paste(contentText,value);
                    }
                }  
            }
            #else if(key == style){
            #    styleVector = c(styleVector,value);
            #}else if(key == content) {
            #    contentText = paste(contentText,value)
            #}
        }   
    } 

    authorTable = as.data.frame(table(authorVector));
    names(authorTable) = c("word","freq");
    authorTable = authorTable[order(-authorTable$freq),];
    wordcloud2(authorTable);
   
    segWorker = worker();

    contentKeyWords = as.data.frame(table(factor(segment(contentText,segWorker))));
    names(contentKeyWords) = c("word","freq");
    contentKeyWords = contentKeyWords[order(-contentKeyWords$freq),]
    contentKeyWords$word = as.character(contentKeyWords$word);
    head(contentKeyWords);
    wordcloud2(contentKeyWords[contentKeyWords$freq > 5 & nchar(contentKeyWords$word) > 1, ]);
}




