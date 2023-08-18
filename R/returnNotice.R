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
#' returnNoticeEcsServer()
returnNoticeEcsServer <- function(input,output,session,dms_token) {
  var_txt_returnNoticeSourceSync_manually <- tsui::var_text('txt_returnNoticeSourceSync_manually')
  var_returnNoticeERP=tsui::var_ListChoose1('returnNoticeERP')
  # var_returnNoticeERP() <- reactive({
  #   switch(input$returnNoticeERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_returnNoticeSourceSync_auto=tsui::var_date('date_returnNoticeSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_returnNoticeSourceSync_log,{
    FNumber <- var_txt_returnNoticeSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入退货通知单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::returnNoticeLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_returnNoticeSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_returnNoticeSourceSync_update,{

    FNumber <- var_txt_returnNoticeSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入退货通知单号")

    }else{
      token= dms_token

      FName <- var_returnNoticeERP()
      mdlCpEcsBillr::returnNoticeStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_returnNoticeSourceSync_manually,{
    FNumber <- var_txt_returnNoticeSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入退货通知单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_returnNoticeSourceSync_manually()
      FName <- var_returnNoticeERP()
      mdlCpEcsBillr::returnNoticeByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$returnNoticeByNumber_query,{
    FNumber <- var_txt_returnNoticeSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入退货通知单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::returnNoticeByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_returnNoticeSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_returnNoticeSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_returnNoticeSourceSync_auto()
    FName <- var_returnNoticeERP()
    mdlCpEcsBillr::returnNoticeByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_returnNoticeSourceSync_query,{

    token= dms_token
    FStartDate=var_date_returnNoticeSourceSync_auto()

    data=mdlCpEcsBillr::returnNoticeByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_returnNoticeSourceSync_dataView',data = data )



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
#' returnNoticeErpServer()
returnNoticeErpServer <- function(input,output,session,dms_token) {
  var_returnNoticeERP2=tsui::var_ListChoose1('returnNoticeERP2')

  # var_returnNoticeERP() <- reactive({
  #   switch(input$returnNoticeERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_returnNoticeERP_manually=tsui::var_text('txt_returnNoticeERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_returnNoticeERP_manually,{
    FNumber=var_txt_returnNoticeERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入退货通知单号")

    }else{
      token= dms_token

      FName=var_returnNoticeERP2()
      data=mdlCpEcsBillr::returnNoticeErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_returnNoticeSourceSync_dataView',data = data )



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
#' returnNoticeServer()
returnNoticeServer <- function(input,output,session,dms_token) {
  #演示功用1
  returnNoticeEcsServer(input,output,session,dms_token)
  returnNoticeErpServer(input,output,session,dms_token)


}
