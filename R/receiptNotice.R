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
#' receiptNoticeEcsServer()
receiptNoticeEcsServer <- function(input,output,session,dms_token) {
  var_txt_receiptNoticeSourceSync_manually <- tsui::var_text('txt_receiptNoticeSourceSync_manually')
  var_receiptNoticeERP=tsui::var_ListChoose1('receiptNoticeERP')
  # var_receiptNoticeERP() <- reactive({
  #   switch(input$receiptNoticeERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_receiptNoticeSourceSync_auto=tsui::var_date('date_receiptNoticeSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_receiptNoticeSourceSync_log,{
    FNumber <- var_txt_receiptNoticeSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入收料通知单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::receiptNoticeLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_receiptNoticeSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_receiptNoticeSourceSync_update,{

    FNumber <- var_txt_receiptNoticeSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入收料通知单号")

    }else{
      token= dms_token

      FName <- var_receiptNoticeERP()
      mdlCpEcsBillr::receiptNoticeStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_receiptNoticeSourceSync_manually,{
    FNumber <- var_txt_receiptNoticeSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入收料通知单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_receiptNoticeSourceSync_manually()
      FName <- var_receiptNoticeERP()
      mdlCpEcsBillr::receiptNoticeByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$receiptNoticeByNumber_query,{
    FNumber <- var_txt_receiptNoticeSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入收料通知单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::receiptNoticeByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_receiptNoticeSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_receiptNoticeSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_receiptNoticeSourceSync_auto()
    FName <- var_receiptNoticeERP()
    mdlCpEcsBillr::receiptNoticeByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_receiptNoticeSourceSync_query,{

    token= dms_token
    FStartDate=var_date_receiptNoticeSourceSync_auto()

    data=mdlCpEcsBillr::receiptNoticeByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_receiptNoticeSourceSync_dataView',data = data )



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
#' receiptNoticeErpServer()
receiptNoticeErpServer <- function(input,output,session,dms_token) {
  var_receiptNoticeERP2=tsui::var_ListChoose1('receiptNoticeERP2')

  # var_receiptNoticeERP() <- reactive({
  #   switch(input$receiptNoticeERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_receiptNoticeERP_manually=tsui::var_text('txt_receiptNoticeERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_receiptNoticeERP_manually,{
    FNumber=var_txt_receiptNoticeERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入收料通知单号")

    }else{
      token= dms_token

      FName=var_receiptNoticeERP2()
      data=mdlCpEcsBillr::receiptNoticeErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_receiptNoticeSourceSync_dataView',data = data )



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
#' receiptNoticeServer()
receiptNoticeServer <- function(input,output,session,dms_token) {
  #演示功用1
  receiptNoticeEcsServer(input,output,session,dms_token)
  receiptNoticeErpServer(input,output,session,dms_token)


}
