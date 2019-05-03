library(JointNets)
data("nip_37_data")
split = train_valid_test_split(nip_37_data,c(0.8,0.1,0.1),10000)

train = split[["train"]]
valid = split[["valid"]]
test = split[["test"]]

v_seeking_length = 500
lambda_range = seq(0.01,10,length.out = 100)
result = QDA_eval(train,valid,test,lambda_range, v_seeking_length, method = "diffee")

result["best test accuracy"]
