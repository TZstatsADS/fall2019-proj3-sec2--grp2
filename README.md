# Project: Can you recognize the emotion from an image of a face? 
<img src="figs/CE.jpg" alt="Compound Emotions" width="500"/>
(Image source: https://www.pnas.org/content/111/15/E1454)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2019

+ Team 2
+ Team members
	+ team member 1 Chen, Yanzhu yc3511@columbia.edu
	+ team member 2 Dai, Sen sd3227@columbia.edu
	+ team member 3 Hu, Hang hh2718@columbia.edu
	+ team member 4 Mathew, Jess jm4742@columbia.edu
	+ team member 5 Zhao, Marshall hz2587@columbia.edu

+ Project summary: In this project, we created a classification engine for facial emotion recognition. 

   We were provided with a set of 2,500 facial images with 22 different emontions. Our client is interested in creating an mobile AI program that accurately recognizes the emotion from facial images. The current practice on our client side is just using boosted decision stumps on facial landmark features in which 6006 features are drawn from raw images and 78 ficucial points and Gradient Boosting Model (GBM) is implemented to train the model and make prediction. We take our client's current practice as our baseline model, which generates an around 43% of accurate rate for predicting emontions. At the same time, 6006 features take loads of time to feed GBM model.
   
   Our group managed to best select informative features by computating unique distance between all pairwised 78 ponits and we ended with 3003 features with this algorithms. Based on all 3003 features, we further selectd top 700 most important features optimalized by BGM algorithms. In addtion, we tried more different advanced model with tunned parameters to train the features, such as GBM, Xgboost, random forest, SVM and Deep Learing model. we noted that SVM and Deep Learing models stood out to our team. Having conducted in-depth research and simulated accuracy rates from different sampled train vs test datasets, we noted that svm performed stably well, whereas deep learing model sometimes did overperformed svm model but it had a great variation. Therefore, We finally choose svm as our training model.
   
   We has achieved an accuracy rate of 53.4% which improves our client's current model and at the same time, our model achieved high efficiency since running time has been greatly reduced. 
   
   
	
**Contribution statement**: All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
