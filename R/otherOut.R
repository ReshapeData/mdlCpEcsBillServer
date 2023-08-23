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
#' otherOutEcsServer()
otherOutEcsServer <- function(input,output,session,dms_token) {
  var_txt_otherOutSourceSync_manually <- tsui::var_text('txt_otherOutSourceSync_manually')
  var_otherOutERP=tsui::var_ListChoose1('otherOutERP')
  # var_otherOutERP() <- reactive({
  #   switch(input$otherOutERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_otherOutSourceSync_auto=tsui::var_date('date_otherOutSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_otherOutSourceSync_log,{
    FNumber <- var_txt_otherOutSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入其他出库单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::otherOutLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_otherOutSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_otherOutSourceSync_update,{

    FNumber <- var_txt_otherOutSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入其他出库单号")

    }else{
      token= dms_token

      FName <- var_otherOutERP()
      mdlCpEcsBillr::otherOutStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_otherOutSourceSync_manually,{
    FNumber <- var_txt_otherOutSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入其他出库单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_otherOutSourceSync_manually()
      FName <- var_otherOutERP()
      mdlCpEcsBillr::otherOutByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$otherOutByNumber_query,{
    FNumber <- var_txt_otherOutSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入其他出库单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::otherOutByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_otherOutSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_otherOutSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_otherOutSourceSync_auto()
    FName <- var_otherOutERP()
    mdlCpEcsBillr::otherOutByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_otherOutSourceSync_query,{

    token= dms_token
    FStartDate=var_date_otherOutSourceSync_auto()

    data=mdlCpEcsBillr::otherOutByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_otherOutSourceSync_dataView',data = data )



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
#' otherOutErpServer()
otherOutErpServer <- function(input,output,session,dms_token) {
  var_otherOutERP2=tsui::var_ListChoose1('otherOutERP2')

  # var_otherOutERP() <- reactive({
  #   switch(input$otherOutERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_otherOutERP_manually=tsui::var_text('txt_otherOutERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_otherOutERP_manually,{
    FNumber=var_txt_otherOutERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入其他出库单号")

    }else{
      token= dms_token

      FName=var_otherOutERP2()
      data=mdlCpEcsBillr::otherOutErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_otherOutSourceSync_dataView',data = data )



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
#' otherOutServer()
otherOutServer <- function(input,output,session,dms_token) {
  #演示功用1
  otherOutEcsServer(input,output,session,dms_token)
  otherOutErpServer(input,output,session,dms_token)


}
