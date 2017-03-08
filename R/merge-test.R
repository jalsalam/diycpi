#merge-test.R

#This is a test file intended for testing out GitKraken steps with slight merge conflicts

#First here, Jameel will change/commit/push. Then Nabeel will change/(not commit)/pull FF if possible:
#Jameel makes the best changes.
#I think this will result in a stash

#Then here, Jameel will change/commit/push. Then Nabeel will change/commit/push. This will give error. So you will pull and it will report merge conflicts. You will resolve the merge conflicts, commit/push.
#[Change this line #2] Nabeel's change that will conflict
# Both are good ideas, let's talk
#Jameel's second change was more questionable.
#This will result in an extra "merge commit"

#3 - Jameel will change/commit/push. Nabeel will change/(not commit)/pull FF if possible.
#Jameel returns to form, another good change!

#4 - Jameel will change/commit/push. Nabeel will change/(not commit)/stash/pull/pop
#This is Jameel's next change.

#5 - this one is different. Now we will have different branches. Jameel will change/commit/push. Nabeel will branch/change/commit/push.
#[change thiS LINE]
#this won't conflict.
