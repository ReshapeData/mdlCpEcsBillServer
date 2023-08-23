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
#' saleOutEcsServer()
saleOutEcsServer <- function(input,output,session,dms_token) {
  var_txt_saleOutSourceSync_manually <- tsui::var_text('txt_saleOutSourceSync_manually')
  var_saleOutERP=tsui::var_ListChoose1('saleOutERP')
  # var_saleOutERP() <- reactive({
  #   switch(input$saleOutERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_saleOutSourceSync_auto=tsui::var_date('date_saleOutSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_saleOutSourceSync_log,{
    FNumber <- var_txt_saleOutSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入销售出库单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::saleOutLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_saleOutSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_saleOutSourceSync_update,{

    FNumber <- var_txt_saleOutSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售出库单号")

    }else{
      token= dms_token

      FName <- var_saleOutERP()
      mdlCpEcsBillr::saleOutStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_saleOutSourceSync_manually,{
    FNumber <- var_txt_saleOutSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售出库单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_saleOutSourceSync_manually()
      FName <- var_saleOutERP()
      mdlCpEcsBillr::saleOutByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$saleOutByNumber_query,{
    FNumber <- var_txt_saleOutSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售出库单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::saleOutByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_saleOutSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_saleOutSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_saleOutSourceSync_auto()
    FName <- var_saleOutERP()
    mdlCpEcsBillr::saleOutByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_saleOutSourceSync_query,{

    token= dms_token
    FStartDate=var_date_saleOutSourceSync_auto()

    data=mdlCpEcsBillr::saleOutByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_saleOutSourceSync_dataView',data = data )



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
#' saleOutErpServer()
saleOutErpServer <- function(input,output,session,dms_token) {
  var_saleOutERP2=tsui::var_ListChoose1('saleOutERP2')

  # var_saleOutERP() <- reactive({
  #   switch(input$saleOutERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_saleOutERP_manually=tsui::var_text('txt_saleOutERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_saleOutERP_manually,{
    FNumber=var_txt_saleOutERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入销售出库单号")

    }else{
      token= dms_token

      FName=var_saleOutERP2()
      data=mdlCpEcsBillr::saleOutErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_saleOutSourceSync_dataView',data = data )



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
#' saleOutServer()
saleOutServer <- function(input,output,session,dms_token) {
  #演示功用1
  saleOutEcsServer(input,output,session,dms_token)
  saleOutErpServer(input,output,session,dms_token)


}
