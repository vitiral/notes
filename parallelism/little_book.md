# Rendevous
a1done = Semaphore(0)
b1done = Semaphore(0)

- Thread A
statement a1
a1done.signal()
b1done.wait()
statement a2

- Thread B
statement b1
b1done.signal()
a1done.wait()
statement b2

# Mutexes
mut = Semaphore(1)

- ThreadA
mut.wait()
count = count + 1
mut.signal()

- ThreadB
mut.wait()
count = count + 1
mut.signal()


# Rendevous 2
rendevous = Semaphore(-n)

- n Threads
rendevous.signal()
rendevous.wait()
Critical section
