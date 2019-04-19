library(JointNets)
data(cancer)
cancerlist = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"), ]),
                  as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"), ]))

split = train_valid_test_split(cancerlist,c(0.6,0.2,0.2),1000)
train = split["train"]
valid = split["valid"]
test = split["test"]

v_seeking_length = 200
lambda_range = seq(0.1,0.3,length.out = 50)
result = QDA_eval(train,valid,test,lambda_range, v_seeking_length, method = "diffee")

result["best test accuracy"]
