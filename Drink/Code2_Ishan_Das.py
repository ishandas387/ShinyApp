import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
from sklearn.manifold import TSNE
import plotly.graph_objs as go
import plotly .offline as offline
import plotly.figure_factory as ff
import plotly.express as px

#define functions to plot elbow and return scaled data
def plot_elbow(dataset,columnToConvert, mean):
    #creating a subset5 without Y as
    subset = dataset;
    print(subset)
    subset.info()
    print(subset.describe())

    # Defining ca
    def converter(column):
        if column <= mean:
            return 0 # low
        else:
            return 1 # high

    subset[columnToConvert] = subset[columnToConvert].apply(converter)
    feature_scaler = StandardScaler()
    subset_scaled = feature_scaler.fit_transform(subset)
    print(subset_scaled)

    # Finding the number of clusters (K) - Elbow Plot Method
    inertia_subset = []
    for i in range(1,11):
        kmeans_subset = KMeans(n_clusters = i, random_state = 100)
        kmeans_subset.fit(subset_scaled)
        inertia_subset.append(kmeans_subset.inertia_)

    plt.plot(range(1, 11), inertia_subset)
    plt.title('The Elbow Plot')
    plt.xlabel('Number of clusters')
    plt.ylabel('Inertia')
    plt.show()
    return subset_scaled

# Importing dataset and examining it
dataset = pd.read_csv("Drink.csv")
print(dataset.head())
print(dataset.shape)
print(dataset.info())
print(dataset.describe())

# Plotting Correlation Heatmap
corrs = dataset.corr()
figure = ff.create_annotated_heatmap(
    z=corrs.values,
    x=list(corrs.columns),
    y=list(corrs.index),
    annotation_text=corrs.round(2).values,
    showscale=True)
offline.plot(figure,filename='corrheatmap_drink.html')


dataset = dataset.drop(['density'],axis=1)
#Try on the full dataset first without any Y
feature_scaler = StandardScaler()
X = feature_scaler.fit_transform(dataset)
# Analysis on Personal Data
# Finding the number of clusters (K) - Elbow Plot Method
inertia = []
for i in range(1,11):
    kmeans = KMeans(n_clusters = i, random_state = 100)
    kmeans.fit(X)
    inertia.append(kmeans.inertia_)
plt.plot(range(1, 11), inertia)
plt.title('The Elbow Plot')
plt.xlabel('Number of clusters')
plt.ylabel('Inertia')
plt.show()

# Running KMeans to generate labels
kmeans = KMeans(n_clusters = 2)
kmeans.fit(X)
# Implementing t-SNE to visualize dataset
tsne = TSNE(n_components = 2, perplexity =40,n_iter=3500)
x_tsne = tsne.fit_transform(X)

fixedacidity = list(dataset['fixed acidity'])
volatileacidity = list(dataset['volatile acidity'])
citricacid = list(dataset['citric acid'])
residualsugar = list(dataset['residual sugar'])
chlorides = list(dataset['chlorides'])
freesulfurdioxide = list(dataset['free sulfur dioxide'])
totalsulfurdioxide = list(dataset['total sulfur dioxide'])
#density = list(dataset['density'])
ph = list(dataset['pH'])
sulphates = list(dataset['sulphates'])
alcohol = list(dataset['alcohol'])
data = [go.Scatter(x=x_tsne[:,0], y=x_tsne[:,1], mode='markers', 
                    marker = dict(color=kmeans.labels_, colorscale='Rainbow', opacity=0.5),
                    text=[f'FA: {a} ; VA: {b}; CA:{c}; RS:{d}; C :{e}; FSD :{f}; TSD :{g};ph :{i};sulphates :{j};alcohol :{k}' for a,b,c,d,e,f,g,i,j,k in list(zip(fixedacidity,volatileacidity,citricacid,residualsugar,chlorides,freesulfurdioxide,totalsulfurdioxide,ph,sulphates,alcohol))],
                    hoverinfo='text')]


layout = go.Layout(title = 't-SNE Dimensionality Reduction', width = 700, height = 700,
                    xaxis = dict(title='First Dimension'),
                    yaxis = dict(title='Second Dimension'))
fig = go.Figure(data=data, layout=layout)
offline.plot(fig,filename='t-SNE_fullset_per40_3500_1.html')

#creating a subset code extracted from ipynb file 
subset8 = dataset[['alcohol','total sulfur dioxide','residual sugar']]
X_subset8_scaled = plot_elbow(subset8, 'total sulfur dioxide', 138 )

# Running KMeans to generate labels
kmeans_subset8 = KMeans(n_clusters = 2)
kmeans_subset8.fit(X_subset8_scaled)
# Implementing t-SNE to visualize dataset
tsne_subset8 = TSNE(n_components = 2, perplexity =150,n_iter=2000)
x_tsne_subset8 = tsne_subset8.fit_transform(X_subset8_scaled)
totalsulphurdioxide = list(subset8['total sulfur dioxide'])
residualsugar = list(subset8['residual sugar'])
al = list(subset8['alcohol'])
data = [go.Scatter(x=x_tsne_subset8[:,0], y=x_tsne_subset8[:,1], mode='markers', 
                    marker = dict(color=kmeans_subset8.labels_, colorscale='Rainbow', opacity=0.5),
                    text=[f'  RS:{b};tsd :{d}; al:{c} ' for b,d,c in list(zip(residualsugar,totalsulphurdioxide,al))],
                    hoverinfo='text')]

layout = go.Layout(title = 't-SNE t-SNE with freesulphur totalsulpd residuals fsd all converted Y= rs', width = 700, height = 700,
                    xaxis = dict(title='First Dimension'),
                    yaxis = dict(title='Second Dimension'))
fig = go.Figure(data=data, layout=layout)
offline.plot(fig,filename='t-SNE__subset8_per150_2000.html')


#creating a subset
subset9 = dataset[['alcohol','total sulfur dioxide','residual sugar','pH','citric acid']]
scaled = plot_elbow(subset9, 'total sulfur dioxide', 138 )
# Running KMeans to generate labels
kmeans_subset9 = KMeans(n_clusters = 3)
kmeans_subset9.fit(scaled)
# Implementing t-SNE to visualize dataset
tsne_subset9 = TSNE(n_components = 2, perplexity =100,n_iter=3000)
x_tsne_subset9 = tsne_subset9.fit_transform(scaled)
ph = list(subset9['pH'])
totalsulphurdioxide = list(subset9['total sulfur dioxide'])
residualsugar = list(subset9['residual sugar'])
al = list(subset9['alcohol'])
ca = list(subset9['citric acid'])

data = [go.Scatter(x=x_tsne_subset9[:,0], y=x_tsne_subset9[:,1], mode='markers', 
                    marker = dict(color=kmeans_subset9.labels_, colorscale='Rainbow', opacity=0.5),
                    text=[f'  RS:{b};tsd :{d}; al:{c}; ph:{e}; ca:{f} ' for b,d,c,e,f in list(zip(residualsugar,totalsulphurdioxide,al,ph,ca))],
                    hoverinfo='text')]

layout = go.Layout(title = 't-SNE t-SNE with  totalsulpd residuals fsd all converted Y= tsd', width = 700, height = 700,
                    xaxis = dict(title='First Dimension'),
                    yaxis = dict(title='Second Dimension'))
fig = go.Figure(data=data, layout=layout)
offline.plot(fig,filename='t-SNE__subset9_per100_2000.html')

#creating a subset
subset10 = dataset[['alcohol','total sulfur dioxide','residual sugar','free sulfur dioxide']]
scaled = plot_elbow(subset10, 'total sulfur dioxide', 138 )

# Running KMeans to generate labels
kmeans_subset10 = KMeans(n_clusters = 3)
kmeans_subset10.fit(scaled)
# Implementing t-SNE to visualize dataset
tsne_subset10 = TSNE(n_components = 2, perplexity =50,n_iter=2000)
x_tsne_subset10 = tsne_subset10.fit_transform(scaled)
totalsulphurdioxide = list(subset10['total sulfur dioxide'])
residualsugar = list(subset10['residual sugar'])
al = list(subset10['alcohol'])
fsd= list(subset10['free sulfur dioxide'])

data = [go.Scatter(x=x_tsne_subset10[:,0], y=x_tsne_subset10[:,1], mode='markers', 
                    marker = dict(color=kmeans_subset10.labels_, colorscale='Rainbow', opacity=0.5),
                    text=[f'  RS:{b};tsd :{d}; al:{c}; fsd:{g} ' for b,d,c,g in list(zip(residualsugar,totalsulphurdioxide,al,fsd))],
                    hoverinfo='text')]

layout = go.Layout(title = 't-SNE t-SNE with  totalsulpd residuals fsd all converted Y= tsd', width = 700, height = 700,
                    xaxis = dict(title='First Dimension'),
                    yaxis = dict(title='Second Dimension'))
fig = go.Figure(data=data, layout=layout)
offline.plot(fig,filename='t-SNE__subset10_per50_2000.html')