echo $1
REMOTE_PATH="lagos.stanford.edu:~/eager_apps/app/gen"

scp -i ~/.ssh/id_rsa tianzhao@$REMOTE_PATH/$1/info/controller_tree.html $1.html
scp -i ~/.ssh/id_rsa tianzhao@$REMOTE_PATH/$1/info/IR.html $1_IR.html
