f=open('filehandling','rt')
num_words=0
num_lines=0
num_char=0
for i in f :
    num_lines +=1
    words =i.split()
    num_words +=len(words)
    num_char=len(i)


print("counting of lines ,words ,character in the file",num_lines,num_words,num_char)
