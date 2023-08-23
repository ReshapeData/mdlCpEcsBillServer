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
#' salesBillingEcsServer()
salesBillingEcsServer <- function(input,output,session,dms_token) {
  var_txt_salesBillingSourceSync_manually <- tsui::var_text('txt_salesBillingSourceSync_manually')
  var_salesBillingERP=tsui::var_ListChoose1('salesBillingERP')
  # var_salesBillingERP() <- reactive({
  #   switch(input$salesBillingERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_salesBillingSourceSync_auto=tsui::var_date('date_salesBillingSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_salesBillingSourceSync_log,{
    FNumber <- var_txt_salesBillingSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入应收单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::salesBillingLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_salesBillingSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_salesBillingSourceSync_update,{

    FNumber <- var_txt_salesBillingSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入应收单号")

    }else{
      token= dms_token

      FName <- var_salesBillingERP()
      mdlCpEcsBillr::salesBillingStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_salesBillingSourceSync_manually,{
    FNumber <- var_txt_salesBillingSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入应收单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_salesBillingSourceSync_manually()
      FName <- var_salesBillingERP()
      mdlCpEcsBillr::salesBillingByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$salesBillingByNumber_query,{
    FNumber <- var_txt_salesBillingSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入应收单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::salesBillingByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_salesBillingSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_salesBillingSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_salesBillingSourceSync_auto()
    FName <- var_salesBillingERP()
    mdlCpEcsBillr::salesBillingByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_salesBillingSourceSync_query,{

    token= dms_token
    FStartDate=var_date_salesBillingSourceSync_auto()

    data=mdlCpEcsBillr::salesBillingByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_salesBillingSourceSync_dataView',data = data )



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
#' salesBillingErpServer()
salesBillingErpServer <- function(input,output,session,dms_token) {
  var_salesBillingERP2=tsui::var_ListChoose1('salesBillingERP2')

  # var_salesBillingERP() <- reactive({
  #   switch(input$salesBillingERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_salesBillingERP_manually=tsui::var_text('txt_salesBillingERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_salesBillingERP_manually,{
    FNumber=var_txt_salesBillingERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入应收单号")

    }else{
      token= dms_token

      FName=var_salesBillingERP2()
      data=mdlCpEcsBillr::salesBillingErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_salesBillingSourceSync_dataView',data = data )



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
#' salesBillingServer()
salesBillingServer <- function(input,output,session,dms_token) {
  #演示功用1
  salesBillingEcsServer(input,output,session,dms_token)
  salesBillingErpServer(input,output,session,dms_token)


}
