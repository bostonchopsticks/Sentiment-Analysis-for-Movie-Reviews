# Sentiment-Analysis-for-Movie-Reviews
Building Model to Analyze Sentiment of Movie Reviews (Kaggle Challenge)
This is a final project that I did in class with 2 other team mates. I was in charge of writing and putting all the codes together while my pals helped with building a wordlist to extract from the reviews. 

# The Rmarkdown file is the combination of 5 separate Rcode file in Rcode branch. I find it easier (for trouble shooting) to divide the project into several small chunks.

## The workflow of this project:
### 1. Split data to train set and validation set
### 2. Text mining with tm package
### 3. Investigate the data
### 4. Build a word list with sentiment score (Using the wordlist as explanatory variables)
### 5. Use several method (Random Forest, SVM, and Neural Networks - nnet package) on validation set
### 6. Pick the best model to test on train + validation set (70% + 30%) 
### 7. Use the model for Test data 
