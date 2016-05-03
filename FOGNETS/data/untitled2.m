Train = readtable('ti.csv');
Test = readtable('tesi.csv');


Y_train = Train.yield;
X_train = Train(:,3:16);                   % select predictor variables
vars = X_train.Properties.VariableNames;    % get variable names
X_train = table2array(X_train);             % convert to a numeric matrix
X_test = table2array(Test(:,3:16));        % convert to a numeric matrix
Y_test=table2array(Test(:,17:17)); 
rng(1);                                     % for reproducibility
c = cvpartition(Y_train,'holdout', 0.20);

B=fitensemble(X_train,Y_train,'Bag',400,'Tree','type','regression');
figure;
plot(loss(B,X_test,Y_test,'mode','cumulative'));
