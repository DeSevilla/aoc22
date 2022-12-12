with open('day1.txt', 'r') as file:
    inputs = file.read()
sublists = map(lambda x: x.split('\n'), inputs.split('\n\n'))


