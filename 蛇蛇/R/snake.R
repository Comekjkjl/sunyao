# task 2.1
library(tuneR)

# 获得矩阵的索引�?
index<-function(col) which(e$m==col)

# 初始化环境变�?
init<-function(){
  e<<-new.env()
  e$stage<-0 #场景
  e$width<-e$height<-20  #切分格子
  e$step<-1/e$width #步长
  e$m<-matrix(rep(0,e$width*e$height),nrow=e$width)  #点矩�?
  e$m[2,3]<-4 # 对蛇头坐标赋�?
  e$dir<-e$lastd<-'up' # 蛇头移动方向
  e$bdir<-"right" # 障碍物移动方�?
  e$head<-c(2,2) #初始蛇头
  e$lastx<-e$lasty<-2 # 初始化蛇头上一个点
  e$block<-c(1,10)  #初始障碍�?
  e$lastbx<-1 # 初始化蛇头上一个点
  e$lastby<-10
  e$tail<-data.frame(x=c(),y=c())#初始蛇尾
  e$num_fruit<-0
  e$if_fruit<-1

  e$col_furit<-2 #水果颜色--红色
  e$col_head<-4 #蛇头颜色--蓝色
  e$col_tail<-8 #蛇尾颜色--灰色
  e$col_block<-9 #障碍物颜�?--黑色
  e$col_path<-0 #路颜�?--白色
}

# 游戏�?
stage1<-function(){
  e$stage<-1

  # 随机的水果点
  furit<-function(){
    if(length(index(e$col_furit))<=0){
      idx<-sample(index(e$col_path),1)

      # 计算水果的坐标并将对应的值变�?2
      fx<-ifelse(idx%%e$width==0,10,idx%%e$width) # idx%%e$width计算余数
      fy<-ceiling(idx/e$height)
      e$m[fx,fy]<-e$col_furit

      print(paste("furit idx",idx))
      print(paste("furit axis:",fx,fy))
    }
  }

  # block
  block<-function(){
    e$lastbx<-e$block[1] # 记录头的横坐�?
    e$lastby<-e$block[2] # 记录头的纵坐�?

    if(e$bdir=='left') {
      e$block[1]<-e$block[1]-1
      if(length(which(e$block[1]<1))>0) {
        e$bdir<-"right"
        e$block[1]<-e$block[1]+1
      }
    }

    if(e$bdir=='right') {
      e$block[1]<-e$block[1]+1
      if(length(which(e$block[1]>20))>0) {
        e$bdir<-"left"
        e$block[1]<-e$block[1]-1
      }
    }
  }

  # 检查失�?
  fail<-function(){
    # head出边�?
    if(length(which(e$head<1))>0 | length(which(e$head>e$width))>0){  # 如果头超出左下或右上
      print("game over: Out of ledge.")
      keydown('q')
      play("C:\Users\Administrator\Desktop\r��ҵ\r��ҵ\snake\R")
      return(TRUE)
    }

    # head碰到tail
    if(e$m[e$head[1],e$head[2]]==e$col_tail){
      print("game over: head hit tail")
      keydown('q')
      play("C:\Users\Administrator\Desktop\r��ҵ\r��ҵ\snake\R")
      return(TRUE)
    }

    # tail碰到block
    if(e$m[e$block[1],e$block[2]]==e$col_tail){
      print("game over: tail hit block.")
      keydown('q')
      play("C:\Users\Administrator\Desktop\r��ҵ\r��ҵ\snake\R")
      return(TRUE)
    }

    # head碰到block
    if(length(index(e$col_head))<=0){
      print("game over: head hit block")
      play("C:\Users\Administrator\Desktop\r��ҵ\r��ҵ\snake\R")
      keydown('q')
      return(TRUE)
    }
    return(FALSE)
  }

  # snake head
  head<-function(){
    e$lastx<-e$head[1] # 记录头的横坐�?
    e$lasty<-e$head[2] # 记录头的纵坐�?

    # 方向操作
    if(e$dir=='up') e$head[2]<-e$head[2]+1  # 如果方向�?"up",则头的纵坐标+1
    if(e$dir=='down') e$head[2]<-e$head[2]-1  # 如果方向�?"down",则头的纵坐标-1
    if(e$dir=='left') e$head[1]<-e$head[1]-1  # 如果方向�?"left",则头的横坐标-1
    if(e$dir=='right') e$head[1]<-e$head[1]+1  # 如果方向�?"right",则头的横坐标+1
    if(e$dir=='p') { }
  }

  # snake body
  body<-function(){
    e$m[e$lastx,e$lasty]<-0 # 原来的头的值变�?0�?--�?
    e$m[e$head[1],e$head[2]]<-e$col_head #得到新头

    e$m[e$lastbx,e$lastby]<-0
    e$m[e$block[1],e$block[2]]<-e$col_block #得到新障碍物

    if(length(index(e$col_furit))<=0){ # 如果蛇吃掉水果后
      e$num_fruit<-e$num_fruit+1
      e$if_fruit<-e$if_fruit+1
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty)) # 吃掉水果后，上一个头变成尾巴
      play("C:\Users\Administrator\Desktop\r��ҵ\r��ҵ\snake\R")
    }

    if(nrow(e$tail)>0) { #如果有尾�?
      if(e$dir=='p'){ }
      else {
        e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty)) # 把原来的头坐标给尾巴
        if(e$if_fruit%%4!=0) {
          e$m[e$tail[1,]$x,e$tail[1,]$y]<-e$col_path # 把尾巴末端删�?
          e$tail<-e$tail[-1,]
        }# 把尾巴某端向量删�?
        if(e$if_fruit==4) e$if_fruit<-1
        e$m[e$lastx,e$lasty]<-e$col_tail
      }
    }
    print(paste("snake idx",index(e$col_head)))
    print(paste("snake axis:",e$head[1],e$head[2]))
  }

  # 画布背景
  drawTable<-function(){
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  }

  # 根据矩阵画数�?
  drawMatrix<-function(){
    idx<-which(e$m>0)
    px<- (ifelse(idx%%e$width==0,e$width,idx%%e$width)-1)/e$width+e$step/2
    py<- (ceiling(idx/e$height)-1)/e$height+e$step/2
    pxy<-data.frame(x=px,y=py,col=e$m[idx])
    points(pxy$x,pxy$y,col=pxy$col,pch=15,cex=4.4)
    text(0.5,0.9,label=paste("You have eat",e$num_fruit,"fruits!"),cex=2,col=4)
  }

  furit()
  block()
  head()
  if(!fail()){
    body()
    drawTable()
    drawMatrix()
  }
}

# 开机画�?
stage0<-function(){
  e$stage<-0
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Snake Game",cex=5)
  text(0.5,0.4,label="Any keyboard to start",cex=2,col=4)
  text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 结束画图
stage2<-function(){
  e$stage<-2
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Game Over",cex=5)
  text(0.5,0.4,label="Space to restart, q to quit.",cex=2,col=4)
  text(0.5,0.3,label=paste("Congratulations! You have eat",e$num_fruit,"fruits!"),cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 暂停画图
stage3<-function(){
  e$stage<-3
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Pause the Game",cex=5)
  text(0.5,0.4,label="p to continue",cex=2,col=4)
  text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 键盘事件
keydown<-function(K){
  print(paste("keydown:",K,",stage:",e$stage));

  if(e$stage==0){ #开机画�?
    init()
    stage1()
    return(NULL)
  }

  if(e$stage==1){ #游戏�?
    if(K == "p") {
      stage3()
    } else if(K == "q"){
      stage2()
    } else {
      if(tolower(K) %in% c("up","down","left","right")){
        # e$lastd<-e$dir
        e$dir<-tolower(K)
        stage1()
      }
    }
    return(NULL)
  }

  if(e$stage==2){ #结束画面
    if(K=="q") q()
    else if(K==' ') stage0()
    return(NULL)
  }

  if(e$stage==3){ #暂停画面
    if(K=="q"){
      stage2()
    }
    else {
      if(K=="p") {
        e$dir<-"p"
        stage1()
      }
    }
    return(NULL)
  }
}


run<-function(){
  par(mai=rep(0,4),oma=rep(0,4))
  e<<-new.env()
  stage0()

  getGraphicsEvent(prompt="Snake Game",onKeybd=keydown)
}

run()
