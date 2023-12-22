# -*- coding: utf-8 -*-
"""
Created on Thu Dec 16 12:02:21 2021

@author: IvanoV
"""
import numpy as np
from scipy import optimize
import matplotlib.pyplot as plt
from random import random
import tensorflow as tf
from sklearn.model_selection import train_test_split
import pandas as pd
import datetime
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.utils import to_categorical 
import random




def generate_dataset(df, df_forecast, group=None):
    
    df["V11"]=df["V11"].astype("int")
    df["V11"]=df["V11"]-1
    df["V11"]=df["V11"].astype("str")

    x = df[["PC1","PC2","PC3"]]
    y = to_categorical(df["V11"])
    if group is not None: 
        y=to_categorical(to_categorical(envy["V11"])[:,group])
    

    # split datasets into test and training sets
    x_train=x
    y_train=y
    x_test=df_forecast[["PC1","PC2","PC3"]]
    y_test=y[range(0,df_forecast.shape[0])]
    return x_train, x_test, y_train, y_test

def plot_history(history):

    fig, axs = plt.subplots(2)

    # accuracy sublpots
    axs[0].plot(history.history["acc"], label="train accuracy")
    axs[0].plot(history.history["val_acc"], label="test accuracy")
    axs[0].set_ylabel("Accuracy")
    axs[0].legend(loc="lower right")
    axs[0].set_title("Accuracy eval")

    # error sublpots
    axs[1].plot(history.history["loss"], label="train error")
    axs[1].plot(history.history["val_loss"], label="test error")
    axs[1].set_ylabel("Error")
    axs[1].set_xlabel("Epoch")
    axs[1].legend(loc="upper right")
    axs[1].set_title("Error eval")

    plt.show()


if __name__ == "__main__":
    for i in range(0,100):
        
        envy = pd.read_excel(
            "my_environment_and_classification.xlsx")

        idx = np.array(random.sample(range(envy.shape[0]),envy.shape[0]))
        envy = envy.iloc[idx.argsort(kind='mergesort')]


        env_frt = pd.read_excel(
            "my_new_environment.xlsx"
            , sheet_name="PCs")


        x_train, x_test, y_train, y_test = generate_dataset(envy, env_frt, group=None)

    # build model with 3 layers: 2 neurons -> 5 neurons -> 1 neuron
    # to change the structure of the model substitute the number of neurons as first parameter in the functions tf.keras.layers.Dense
        model = tf.keras.models.Sequential([
            tf.keras.layers.Dense(10, input_dim=x_train.shape[1], activation="relu", kernel_regularizer=keras.regularizers.l2(0.02)),
            tf.keras.layers.Dropout(0.1),
            tf.keras.layers.Dense(100, activation="relu", kernel_regularizer=keras.regularizers.l2(0.02)),
            tf.keras.layers.Dropout(0.2),
            tf.keras.layers.Dense(10, activation="relu"),
            tf.keras.layers.Dense(y_train.shape[1], activation="softmax")

            ])

        # choose optimiser
        callback = tf.keras.callbacks.EarlyStopping(monitor='loss', patience=10)
        model.compile(optimizer='adam', 
                      loss='categorical_crossentropy', 
                      metrics=['accuracy'])

        # train model
        history = model.fit(x_train, y_train, validation_split = 0.2,
                            batch_size= 20, callbacks=[callback], epochs=100)
        plot_history(history)
        
        # evaluate model on test set
        print("\nEvaluation on the test set:")
        model.evaluate(x_test,  y_test, verbose=2)

        # get predictions
        data = x_test
        datapiran = x_train
        predictions = model.predict(data)
        predictionspiran= model.predict(datapiran)
        
        predictions = pd.DataFrame(predictions, index=[env_frt["Unnamed: 0"]])
        if i==0:
            predictions.to_excel('prediction.xlsx', sheet_name=str(i), index=True)
        else:
            with pd.ExcelWriter('prediction.xlsx' , mode="a") as writer: 
                predictions.to_excel(writer, sheet_name=str(i), index=True)
                
        predictionspiran = pd.DataFrame(predictionspiran, index=[envy["Unnamed: 0"]])
        if i==0:
            predictionspiran.to_excel('prediction_test.xlsx.xlsx', sheet_name=str(i), index=True)
        else:
            with pd.ExcelWriter('prediction_test.xlsx' , mode="a") as writer: 
                predictionspiran.to_excel(writer, sheet_name=str(i), index=True)
        #print(np.around(predictions, decimals=2))
        print(i)
        #print(y_test)

    
