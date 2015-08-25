import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_csv('http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv', 
                   index_col=0)
data.head()
data.shape

# visualize the relationship between the features and the response using 
# scatterplots
fig, axs = plt.subplots(1, 3, sharey=True)
data.plot(kind='scatter', x='TV', y='Sales', ax=axs[0], figsize=(16, 8))
data.plot(kind='scatter', x='Radio', y='Sales', ax=axs[1])
data.plot(kind='scatter', x='Newspaper', y='Sales', ax=axs[2])

# this is the standard import if you're using "formula notation" (similar to R)
import statsmodels.formula.api as smf

# create a fitted model in one line
lm = smf.ols(formula='Sales ~ TV', data=data).fit()

# print the coefficients
lm.params

preds = lm.predict(pd.DataFrame({'TV': [50]}))
