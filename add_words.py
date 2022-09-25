f = open('english', 'r')
all_words = f.read().split()
f.close()


new_words = input('Enter words to add: ').split()

ignored = []
added = []
for word in new_words:
    if word in all_words:
        ignored.append(word)
    else:
        added.append(word)

print('\n')

print('Ignored %d words:' % len(ignored))
print('\t%s\n' % ' '.join(ignored))

print('Added %d words:' % len(added))
print('\t%s\n' % ' '.join(added))


f = open('english', 'w')
f.write(' '.join(all_words + added))
f.close()