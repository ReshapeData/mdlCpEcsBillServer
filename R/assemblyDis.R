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
#' assemblyDisEcsServer()
assemblyDisEcsServer <- function(input,output,session,dms_token) {
  var_txt_assemblyDisSourceSync_manually <- tsui::var_text('txt_assemblyDisSourceSync_manually')
  var_assemblyDisERP=tsui::var_ListChoose1('assemblyDisERP')
  # var_assemblyDisERP() <- reactive({
  #   switch(input$assemblyDisERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_assemblyDisSourceSync_auto=tsui::var_date('date_assemblyDisSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_assemblyDisSourceSync_log,{
    FNumber <- var_txt_assemblyDisSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入组装拆卸单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::assemblyDisLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_assemblyDisSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_assemblyDisSourceSync_update,{

    FNumber <- var_txt_assemblyDisSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入组装拆卸单号")

    }else{
      token= dms_token

      FName <- var_assemblyDisERP()
      mdlCpEcsBillr::assemblyDisStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_assemblyDisSourceSync_manually,{
    FNumber <- var_txt_assemblyDisSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入组装拆卸单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_assemblyDisSourceSync_manually()
      FName <- var_assemblyDisERP()
      mdlCpEcsBillr::assemblyDisByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$assemblyDisByNumber_query,{
    FNumber <- var_txt_assemblyDisSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入组装拆卸单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::assemblyDisByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_assemblyDisSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_assemblyDisSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_assemblyDisSourceSync_auto()
    FName <- var_assemblyDisERP()
    mdlCpEcsBillr::assemblyDisByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_assemblyDisSourceSync_query,{

    token= dms_token
    FStartDate=var_date_assemblyDisSourceSync_auto()

    data=mdlCpEcsBillr::assemblyDisByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_assemblyDisSourceSync_dataView',data = data )



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
#' assemblyDisErpServer()
assemblyDisErpServer <- function(input,output,session,dms_token) {
  var_assemblyDisERP2=tsui::var_ListChoose1('assemblyDisERP2')

  # var_assemblyDisERP() <- reactive({
  #   switch(input$assemblyDisERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_assemblyDisERP_manually=tsui::var_text('txt_assemblyDisERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_assemblyDisERP_manually,{
    FNumber=var_txt_assemblyDisERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入组装拆卸单号")

    }else{
      token= dms_token

      FName=var_assemblyDisERP2()
      data=mdlCpEcsBillr::assemblyDisErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_assemblyDisSourceSync_dataView',data = data )



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
#' assemblyDisServer()
assemblyDisServer <- function(input,output,session,dms_token) {
  #演示功用1
  assemblyDisEcsServer(input,output,session,dms_token)
  assemblyDisErpServer(input,output,session,dms_token)


}
