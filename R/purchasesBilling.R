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
#' purchasesBillingEcsServer()
purchasesBillingEcsServer <- function(input,output,session,dms_token) {
  var_txt_purchasesBillingSourceSync_manually <- tsui::var_text('txt_purchasesBillingSourceSync_manually')
  var_purchasesBillingERP=tsui::var_ListChoose1('purchasesBillingERP')
  # var_purchasesBillingERP() <- reactive({
  #   switch(input$purchasesBillingERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_purchasesBillingSourceSync_auto=tsui::var_date('date_purchasesBillingSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_purchasesBillingSourceSync_log,{
    FNumber <- var_txt_purchasesBillingSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入应付单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::purchasesBillingLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_purchasesBillingSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_purchasesBillingSourceSync_update,{

    FNumber <- var_txt_purchasesBillingSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入应付单号")

    }else{
      token= dms_token

      FName <- var_purchasesBillingERP()
      mdlCpEcsBillr::purchasesBillingStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_purchasesBillingSourceSync_manually,{
    FNumber <- var_txt_purchasesBillingSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入应付单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_purchasesBillingSourceSync_manually()
      FName <- var_purchasesBillingERP()
      mdlCpEcsBillr::purchasesBillingByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$purchasesBillingByNumber_query,{
    FNumber <- var_txt_purchasesBillingSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入应付单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::purchasesBillingByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_purchasesBillingSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_purchasesBillingSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_purchasesBillingSourceSync_auto()
    FName <- var_purchasesBillingERP()
    mdlCpEcsBillr::purchasesBillingByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_purchasesBillingSourceSync_query,{

    token= dms_token
    FStartDate=var_date_purchasesBillingSourceSync_auto()

    data=mdlCpEcsBillr::purchasesBillingByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_purchasesBillingSourceSync_dataView',data = data )



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
#' purchasesBillingErpServer()
purchasesBillingErpServer <- function(input,output,session,dms_token) {
  var_purchasesBillingERP2=tsui::var_ListChoose1('purchasesBillingERP2')

  # var_purchasesBillingERP() <- reactive({
  #   switch(input$purchasesBillingERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_purchasesBillingERP_manually=tsui::var_text('txt_purchasesBillingERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_purchasesBillingERP_manually,{
    FNumber=var_txt_purchasesBillingERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入应付单号")

    }else{
      token= dms_token

      FName=var_purchasesBillingERP2()
      data=mdlCpEcsBillr::purchasesBillingErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_purchasesBillingSourceSync_dataView',data = data )



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
#' purchasesBillingServer()
purchasesBillingServer <- function(input,output,session,dms_token) {
  #演示功用1
  purchasesBillingEcsServer(input,output,session,dms_token)
  purchasesBillingErpServer(input,output,session,dms_token)


}
