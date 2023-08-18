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
#' returnSaleEcsServer()
returnSaleEcsServer <- function(input,output,session,dms_token) {
  var_txt_returnSaleSourceSync_manually <- tsui::var_text('txt_returnSaleSourceSync_manually')
  var_returnSaleERP=tsui::var_ListChoose1('returnSaleERP')
  # var_returnSaleERP() <- reactive({
  #   switch(input$returnSaleERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_returnSaleSourceSync_auto=tsui::var_date('date_returnSaleSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_returnSaleSourceSync_log,{
    FNumber <- var_txt_returnSaleSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入销售退货单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::returnSaleLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_returnSaleSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_returnSaleSourceSync_update,{

    FNumber <- var_txt_returnSaleSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售退货单号")

    }else{
      token= dms_token

      FName <- var_returnSaleERP()
      mdlCpEcsBillr::returnSaleStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_returnSaleSourceSync_manually,{
    FNumber <- var_txt_returnSaleSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售退货单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_returnSaleSourceSync_manually()
      FName <- var_returnSaleERP()
      mdlCpEcsBillr::returnSaleByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$returnSaleByNumber_query,{
    FNumber <- var_txt_returnSaleSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售退货单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::returnSaleByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_returnSaleSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_returnSaleSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_returnSaleSourceSync_auto()
    FName <- var_returnSaleERP()
    mdlCpEcsBillr::returnSaleByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_returnSaleSourceSync_query,{

    token= dms_token
    FStartDate=var_date_returnSaleSourceSync_auto()

    data=mdlCpEcsBillr::returnSaleByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_returnSaleSourceSync_dataView',data = data )



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
#' returnSaleErpServer()
returnSaleErpServer <- function(input,output,session,dms_token) {
  var_returnSaleERP2=tsui::var_ListChoose1('returnSaleERP2')

  # var_returnSaleERP() <- reactive({
  #   switch(input$returnSaleERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_returnSaleERP_manually=tsui::var_text('txt_returnSaleERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_returnSaleERP_manually,{
    FNumber=var_txt_returnSaleERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入销售退货单号")

    }else{
      token= dms_token

      FName=var_returnSaleERP2()
      data=mdlCpEcsBillr::returnSaleErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_returnSaleSourceSync_dataView',data = data )



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
#' returnSaleServer()
returnSaleServer <- function(input,output,session,dms_token) {
  #演示功用1
  returnSaleEcsServer(input,output,session,dms_token)
  returnSaleErpServer(input,output,session,dms_token)


}
