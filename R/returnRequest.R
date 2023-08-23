#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' returnRequestEcsServer()
returnRequestEcsServer <- function(input,output,session,dms_token) {
  var_txt_returnRequestSourceSync_manually <- tsui::var_text('txt_returnRequestSourceSync_manually')
  var_returnRequestERP=tsui::var_ListChoose1('returnRequestERP')
  # var_returnRequestERP() <- reactive({
  #   switch(input$returnRequestERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_returnRequestSourceSync_auto=tsui::var_date('date_returnRequestSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_returnRequestSourceSync_log,{
    FNumber <- var_txt_returnRequestSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入退料申请单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::returnRequestLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_returnRequestSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_returnRequestSourceSync_update,{

    FNumber <- var_txt_returnRequestSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入退料申请单号")

    }else{
      token= dms_token

      FName <- var_returnRequestERP()
      mdlCpEcsBillr::returnRequestStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_returnRequestSourceSync_manually,{
    FNumber <- var_txt_returnRequestSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入退料申请单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_returnRequestSourceSync_manually()
      FName <- var_returnRequestERP()
      mdlCpEcsBillr::returnRequestByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$returnRequestByNumber_query,{
    FNumber <- var_txt_returnRequestSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入退料申请单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::returnRequestByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_returnRequestSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_returnRequestSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_returnRequestSourceSync_auto()
    FName <- var_returnRequestERP()
    mdlCpEcsBillr::returnRequestByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_returnRequestSourceSync_query,{

    token= dms_token
    FStartDate=var_date_returnRequestSourceSync_auto()

    data=mdlCpEcsBillr::returnRequestByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_returnRequestSourceSync_dataView',data = data )



  })

}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' returnRequestErpServer()
returnRequestErpServer <- function(input,output,session,dms_token) {
  var_returnRequestERP2=tsui::var_ListChoose1('returnRequestERP2')

  # var_returnRequestERP() <- reactive({
  #   switch(input$returnRequestERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_returnRequestERP_manually=tsui::var_text('txt_returnRequestERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_returnRequestERP_manually,{
    FNumber=var_txt_returnRequestERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入退料申请单号")

    }else{
      token= dms_token

      FName=var_returnRequestERP2()
      data=mdlCpEcsBillr::returnRequestErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_returnRequestSourceSync_dataView',data = data )



    }




  })


}







#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' returnRequestServer()
returnRequestServer <- function(input,output,session,dms_token) {
  #演示功用1
  returnRequestEcsServer(input,output,session,dms_token)
  returnRequestErpServer(input,output,session,dms_token)


}
