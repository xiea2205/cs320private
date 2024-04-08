def decode(message_file):
    f = open(message_file, "r")
    dict = {}
    for line in f:
        x = line.split()
        dict[x[0]] = x[1]
    total = 1
    increment = 2
    output_arr = []
    while total < max(dict.keys()):
        output_arr.append(dict[total])
        total += increment
        increment += 1
    return " ".join(output_arr)
    
print(decode("coding_qual_input.txt", 'r'))