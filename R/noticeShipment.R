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
#' noticeShipmentEcsServer()
noticeShipmentEcsServer <- function(input,output,session,dms_token) {
  var_txt_noticeShipmentSourceSync_manually <- tsui::var_text('txt_noticeShipmentSourceSync_manually')
  var_noticeShipmentERP=tsui::var_ListChoose1('noticeShipmentERP')
  # var_noticeShipmentERP() <- reactive({
  #   switch(input$noticeShipmentERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_noticeShipmentSourceSync_auto=tsui::var_date('date_noticeShipmentSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_noticeShipmentSourceSync_log,{
    FNumber <- var_txt_noticeShipmentSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入发货通知单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::noticeShipmentLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_noticeShipmentSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_noticeShipmentSourceSync_update,{

    FNumber <- var_txt_noticeShipmentSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入发货通知单号")

    }else{
      token= dms_token

      FName <- var_noticeShipmentERP()
      mdlCpEcsBillr::noticeShipmentStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_noticeShipmentSourceSync_manually,{
    FNumber <- var_txt_noticeShipmentSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入发货通知单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_noticeShipmentSourceSync_manually()
      FName <- var_noticeShipmentERP()
      mdlCpEcsBillr::noticeShipmentByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$noticeShipmentByNumber_query,{
    FNumber <- var_txt_noticeShipmentSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入发货通知单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::noticeShipmentByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_noticeShipmentSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_noticeShipmentSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_noticeShipmentSourceSync_auto()
    FName <- var_noticeShipmentERP()
    mdlCpEcsBillr::noticeShipmentByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_noticeShipmentSourceSync_query,{

    token= dms_token
    FStartDate=var_date_noticeShipmentSourceSync_auto()

    data=mdlCpEcsBillr::noticeShipmentByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_noticeShipmentSourceSync_dataView',data = data )



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
#' noticeShipmentErpServer()
noticeShipmentErpServer <- function(input,output,session,dms_token) {
  var_noticeShipmentERP2=tsui::var_ListChoose1('noticeShipmentERP2')

  # var_noticeShipmentERP() <- reactive({
  #   switch(input$noticeShipmentERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_noticeShipmentERP_manually=tsui::var_text('txt_noticeShipmentERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_noticeShipmentERP_manually,{
    FNumber=var_txt_noticeShipmentERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入发货通知单号")

    }else{
      token= dms_token

      FName=var_noticeShipmentERP2()
      data=mdlCpEcsBillr::noticeShipmentErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_noticeShipmentSourceSync_dataView',data = data )



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
#' noticeShipmentServer()
noticeShipmentServer <- function(input,output,session,dms_token) {
  #演示功用1
  noticeShipmentEcsServer(input,output,session,dms_token)
  noticeShipmentErpServer(input,output,session,dms_token)


}
