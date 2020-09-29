f=open('filehandling','w')
f.write("Hello, count the total no of words in this file")

f=open('filehandling','r')
num_words=0
for i in f :
    words =i.split()
    num_words +=len(words)
print("counting of words  in the file",num_words)
