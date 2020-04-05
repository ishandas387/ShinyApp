#import the required libraries.
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import plotly .offline as offline
import plotly.figure_factory as ff
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn import tree
from sklearn.model_selection import GridSearchCV
from sklearn import metrics
from imblearn.over_sampling import SMOTE  
from sklearn.ensemble import RandomForestClassifier

def print_conf_matrix(Y_test, Y_pred, title):
    conf_mat = metrics.confusion_matrix(Y_test, Y_pred)
    print('Confusion matrix: \n', conf_mat)
    print(title +'\n')
    print('TP: ', conf_mat[1,1])
    print('TN: ', conf_mat[0,0])
    print('FP: ', conf_mat[0,1])
    print('FN: ', conf_mat[1,0])


# Importing dataset and examining it
dataset = pd.read_csv("Marketing.csv")
print(dataset.head())
print(dataset.shape)
print(dataset.info())
print(dataset.describe())

# Total missing values for each feature
print (dataset.isnull().sum())


# Plotting Correlation Heatmap
corrs = dataset.corr()
figure = ff.create_annotated_heatmap(
    z=corrs.values,
    x=list(corrs.columns),
    y=list(corrs.index),
    annotation_text=corrs.round(2).values,
    showscale=True)
offline.plot(figure,filename='corrheatmap_marketingdata_withoutchange.html')


#data conversion

# Converting Categorical features into Numerical features
dataset['subscribed'] = dataset['subscribed'].map({'yes':1, 'no':0})
dataset['month'] = dataset['month'].map({'jan':1, 'feb':2, 'mar':3,'apr':4, 'may':5,'jun':6, 'jul':7, 'aug':8,'sep':9,'oct':10,'nov':11,'dec':12})
dataset['loan'] = dataset['loan'].map({'yes':1, 'no':0})
dataset['default'] = dataset['default'].map({'yes':1, 'no':0})
dataset['housing'] = dataset['housing'].map({'yes':1, 'no':0})

#non related categorical values to be converted by dummies. #one hot encoding
categorical_features = ['contact', 'marital', 'education', 'job', 'poutcome']
dataset = pd.get_dummies(dataset, columns = categorical_features)


# Defining pdays
def converter(column):
    if column < 0:
        return 0 # never contacted
    elif column > 0 and column < 40:
        return 1 # contacted
    else:
        return 2 #contacted long back
dataset['pdays'] = dataset['pdays'].apply(converter)


print(dataset.head())
print(dataset.tail())



#some plots to check

sns.distplot(dataset['age'], color='red')
sns.barplot(x="housing", y="subscribed", data=dataset);
plt.figure(figsize=(30, 10))
sns.barplot(x="subscribed", y="duration", data=dataset);
plt.figure(figsize=(16, 6))
sns.barplot(x="campaign", y="subscribed", data=dataset);
plt.figure(figsize=(16, 6))
sns.barplot(x="subscribed", y="balance", data=dataset);
plt.figure(figsize=(16, 6))
sns.barplot(x="day", y="subscribed", data=dataset);
plt.figure(figsize=(16, 6))
sns.barplot(x="age", y="subscribed", data=dataset);
sns.boxplot(x="subscribed", y="age", data=dataset, color ='cyan');
sns.barplot(x="poutcome_success", y="subscribed", data=dataset);
plt.figure(figsize=(16, 6))
sns.barplot(x="previous", y="subscribed", data=dataset);

# Plotting Correlation Heatmap
corrs = dataset.corr()
figure = ff.create_annotated_heatmap(
    z=corrs.values,
    x=list(corrs.columns),
    y=list(corrs.index),
    annotation_text=corrs.round(2).values,
    showscale=True)
offline.plot(figure,filename='corrheatmap_marketingdata.html')


#drop p_outcome_unknown and p_out _other since both of them dont give any significant outcome. we only look for suceess failure for our model.

dataset = dataset.drop(['poutcome_unknown'],axis=1)
dataset = dataset.drop(['poutcome_other'],axis=1)
#removing contact unknown as well since it seems to be of no use.
dataset = dataset.drop(['contact_unknown'],axis=1)


# Dividing dataset into label and feature sets
X = dataset.drop(['subscribed'], axis = 1) # Features
Y = dataset['subscribed'] # Labels
print(type(X))
print(type(Y))
print(X.shape)
print(Y.shape)
print(dataset)

# Normalizing numerical features so that each feature has mean 0 and variance 1
feature_scaler = StandardScaler()
X_scaled = feature_scaler.fit_transform(X)

# Dividing dataset into training and test sets
X_train, X_test, Y_train, Y_test = train_test_split( X_scaled, Y, test_size = 0.25, random_state = 100)

print(X_train.shape)
print(X_test.shape)
# Implementing Oversampling to balance the dataset; SMOTE stands for Synthetic Minority Oversampling TEchnique
print("Number of observations in each class before oversampling (training data): \n", pd.Series(Y_train).value_counts())

smote = SMOTE(random_state = 101)
X_train,Y_train = smote.fit_sample(X_train,Y_train)

print("Number of observations in each class after oversampling (training data): \n", pd.Series(Y_train).value_counts())

# Tuning the random forest parameter 'n_estimators' and implementing cross-validation using Grid Search
rfc = RandomForestClassifier(criterion='entropy', max_features='auto', random_state=1)
grid_param = {'n_estimators': [50, 100, 150, 200, 250, 300, 350, 400, 450, 500]}

gd_sr = GridSearchCV(estimator=rfc, param_grid=grid_param, scoring='recall', cv=5)

"""
In the above GridSearchCV(), scoring parameter should be set as follows:
scoring = 'accuracy' when you want to maximize prediction accuracy
scoring = 'recall' when you want to minimize false negatives
scoring = 'precision' when you want to minimize false positives
scoring = 'f1' when you want to balance false positives and false negatives (place equal emphasis on minimizing both)
"""

gd_sr.fit(X_train, Y_train)

best_parameters = gd_sr.best_params_
print(best_parameters)

best_result = gd_sr.best_score_ # Mean cross-validated score of the best_estimator
print(best_result)

# Building random forest using the tuned parameter
rfc = RandomForestClassifier(n_estimators=450, criterion='entropy', max_features='auto', random_state=12)
rfc.fit(X_train,Y_train)
featimp = pd.Series(rfc.feature_importances_, index=list(X)).sort_values(ascending=False)
print(featimp)

Y_pred = rfc.predict(X_test)
conf_mat = metrics.confusion_matrix(Y_test, Y_pred)
#plt.figure(figsize=(5,5))
#sns.heatmap(conf_mat,annot=True)
#plt.title("Confusion_matrix")
#plt.xlabel("Predicted Class")
#plt.ylabel("Actual class")
#plt.show()
print('Confusion matrix: \n', conf_mat)
print('TP: ', conf_mat[1,1])
print('TN: ', conf_mat[0,0])
print('FP: ', conf_mat[0,1])
print('FN: ', conf_mat[1,0])


# Selecting features with higher sifnificance and redefining feature set
X_subset1 = dataset[['duration', 'month', 'campaign', 'balance','day','age','housing','contact_cellular']]
#,'contact','job','education']]

print(X_subset1)

feature_scaler = StandardScaler()
X_subset1 = feature_scaler.fit_transform(X_subset1)

# Dividing dataset into training and test sets
X_train_sub, X_test_sub, Y_train_sub, Y_test_sub = train_test_split( X_subset1, Y, test_size = 0.25, random_state = 100)

smote = SMOTE(random_state = 101)
X_train_sub,Y_train_sub = smote.fit_sample(X_train_sub,Y_train_sub)

rfc = RandomForestClassifier(n_estimators=450, criterion='entropy', max_features='auto', random_state=12)
rfc.fit(X_train_sub,Y_train_sub)

Y_pred_sub = rfc.predict(X_test_sub)
print_conf_matrix(Y_test_sub,Y_pred_sub,'subset')


# Selecting features with higher sifnificance and redefining feature set with 1 less param
X_subset2 = dataset[['duration', 'month', 'campaign', 'balance','day','age','housing']]
#,'contact','job','education']]

print(X_subset2)

feature_scaler = StandardScaler()
X_subset2 = feature_scaler.fit_transform(X_subset2)

# Dividing dataset into training and test sets
X_train_sub2, X_test_sub2, Y_train_sub2, Y_test_sub2 = train_test_split( X_subset2, Y, test_size = 0.25, random_state = 100)

smote = SMOTE(random_state = 101)
X_train_sub2,Y_train_sub2 = smote.fit_sample(X_train_sub2,Y_train_sub2)

rfc = RandomForestClassifier(n_estimators=450, criterion='entropy', max_features='auto', random_state=12)
rfc.fit(X_train_sub2,Y_train_sub2)

Y_pred_sub2 = rfc.predict(X_test_sub2)
print_conf_matrix(Y_test_sub2,Y_pred_sub2,'subset2')

# Selecting features with higher sifnificance and redefining feature set with 1 more  param
X_subset3 = dataset[['duration', 'month', 'campaign', 'balance','day','age','housing','contact_cellular','poutcome_success']]
#,'contact','job','education']]

print(X_subset3)

feature_scaler = StandardScaler()
X_subset3 = feature_scaler.fit_transform(X_subset3)

# Dividing dataset into training and test sets
X_train_sub3, X_test_sub3, Y_train_sub3, Y_test_sub3 = train_test_split( X_subset3, Y, test_size = 0.25, random_state = 100)

smote = SMOTE(random_state = 101)
X_train_sub3,Y_train_sub3 = smote.fit_sample(X_train_sub3,Y_train_sub3)

rfc = RandomForestClassifier(n_estimators=450, criterion='entropy', max_features='auto', random_state=12)
rfc.fit(X_train_sub3,Y_train_sub3)

Y_pred_sub3 = rfc.predict(X_test_sub3)
print_conf_matrix(Y_test_sub3,Y_pred_sub3,'subset3')


# Selecting features with higher sifnificance and redefining feature set with 1 more  param
X_subset4 = dataset[['duration', 'month', 'campaign', 'balance','day','age','housing','contact_cellular','poutcome_success','previous']]
#,'contact','job','education']]

print(X_subset4)

feature_scaler = StandardScaler()
X_subset4 = feature_scaler.fit_transform(X_subset4)

# Dividing dataset into training and test sets
X_train_sub4, X_test_sub4, Y_train_sub4, Y_test_sub4 = train_test_split( X_subset4, Y, test_size = 0.25, random_state = 100)

smote = SMOTE(random_state = 101)
X_train_sub4,Y_train_sub4 = smote.fit_sample(X_train_sub4,Y_train_sub4)

rfc = RandomForestClassifier(n_estimators=450, criterion='entropy', max_features='auto', random_state=12)
rfc.fit(X_train_sub4,Y_train_sub4)

Y_pred_sub4 = rfc.predict(X_test_sub4)
print_conf_matrix(Y_test_sub4,Y_pred_sub4,'subset4')
