Train=csvread('Rangtrain.csv');
Test=csvread('Rangtest.csv');

Y_train = Train.Active_Customer;    
X_train = Train(:,1:255);                   % select predictor variables
X_test = Test(:,1:255);      

categoricalPredictors = {'Cust_status','Trans24','Trans25','Trans26','Trans27','Promotion37','Active_Customer'};
t = templateTree('Surrogate','on');
ens = fitensemble(X_train,Y_train,'Logitboost',250,t);
pred=ens.predict(X_test);
