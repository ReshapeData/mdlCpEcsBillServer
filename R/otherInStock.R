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
#' otherInStockEcsServer()
otherInStockEcsServer <- function(input,output,session,dms_token) {
  var_txt_otherInStockSourceSync_manually <- tsui::var_text('txt_otherInStockSourceSync_manually')
  var_otherInStockERP=tsui::var_ListChoose1('otherInStockERP')
  # var_otherInStockERP() <- reactive({
  #   switch(input$otherInStockERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_otherInStockSourceSync_auto=tsui::var_date('date_otherInStockSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_otherInStockSourceSync_log,{
    FNumber <- var_txt_otherInStockSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入其他入库单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::otherInStockLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_otherInStockSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_otherInStockSourceSync_update,{

    FNumber <- var_txt_otherInStockSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入其他入库单号")

    }else{
      token= dms_token

      FName <- var_otherInStockERP()
      mdlCpEcsBillr::otherInStockStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_otherInStockSourceSync_manually,{
    FNumber <- var_txt_otherInStockSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入其他入库单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_otherInStockSourceSync_manually()
      FName <- var_otherInStockERP()
      mdlCpEcsBillr::otherInStockByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$otherInStockByNumber_query,{
    FNumber <- var_txt_otherInStockSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入其他入库单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::otherInStockByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_otherInStockSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_otherInStockSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_otherInStockSourceSync_auto()
    FName <- var_otherInStockERP()
    mdlCpEcsBillr::otherInStockByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_otherInStockSourceSync_query,{

    token= dms_token
    FStartDate=var_date_otherInStockSourceSync_auto()

    data=mdlCpEcsBillr::otherInStockByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_otherInStockSourceSync_dataView',data = data )



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
#' otherInStockErpServer()
otherInStockErpServer <- function(input,output,session,dms_token) {
  var_otherInStockERP2=tsui::var_ListChoose1('otherInStockERP2')

  # var_otherInStockERP() <- reactive({
  #   switch(input$otherInStockERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_otherInStockERP_manually=tsui::var_text('txt_otherInStockERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_otherInStockERP_manually,{
    FNumber=var_txt_otherInStockERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入其他入库单号")

    }else{
      token= dms_token

      FName=var_otherInStockERP2()
      data=mdlCpEcsBillr::otherInStockErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_otherInStockSourceSync_dataView',data = data )



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
#' otherInStockServer()
otherInStockServer <- function(input,output,session,dms_token) {
  #演示功用1
  otherInStockEcsServer(input,output,session,dms_token)
  otherInStockErpServer(input,output,session,dms_token)


}
