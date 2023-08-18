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
#' saleOrderEcsServer()
saleOrderEcsServer <- function(input,output,session,dms_token) {
  var_txt_saleOrderSourceSync_manually <- tsui::var_text('txt_saleOrderSourceSync_manually')
  var_saleOrderERP=tsui::var_ListChoose1('saleOrderERP')
  # var_saleOrderERP() <- reactive({
  #   switch(input$saleOrderERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_saleOrderSourceSync_auto=tsui::var_date('date_saleOrderSourceSync_auto')
#日志查询
  shiny::observeEvent(input$btn_saleOrderSourceSync_log,{
    FNumber <- var_txt_saleOrderSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入销售订单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::saleOrderLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_saleOrderSourceSync_dataView',data = data )

    }

  })
#更新同步状态
  shiny::observeEvent(input$btn_saleOrderSourceSync_update,{

    FNumber <- var_txt_saleOrderSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售订单号")

    }else{
      token= dms_token

      FName <- var_saleOrderERP()
      mdlCpEcsBillr::saleOrderStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

#按单同步

  shiny::observeEvent(input$btn_saleOrderSourceSync_manually,{
    FNumber <- var_txt_saleOrderSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售订单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_saleOrderSourceSync_manually()
      FName <- var_saleOrderERP()
      mdlCpEcsBillr::saleOrderByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$saleOrderByNumber_query,{
    FNumber <- var_txt_saleOrderSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入销售订单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::saleOrderByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_saleOrderSourceSync_dataView',data = data )

    }

  })
#手动同步
  shiny::observeEvent(input$btn_saleOrderSourceSync_auto,{

      Ftoken= dms_token
      FDate=var_date_saleOrderSourceSync_auto()
      FName <- var_saleOrderERP()
      mdlCpEcsBillr::saleOrderByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
      tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_saleOrderSourceSync_query,{

      token= dms_token
      FStartDate=var_date_saleOrderSourceSync_auto()

      data=mdlCpEcsBillr::saleOrderByDate_query(token = token,FStartDate =FStartDate )
      tsui::run_dataTable2(id ='dt_saleOrderSourceSync_dataView',data = data )



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
#' saleOrderErpServer()
saleOrderErpServer <- function(input,output,session,dms_token) {
  var_saleOrderERP2=tsui::var_ListChoose1('saleOrderERP2')

  # var_saleOrderERP() <- reactive({
  #   switch(input$saleOrderERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_saleOrderERP_manually=tsui::var_text('txt_saleOrderERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_saleOrderERP_manually,{
    FNumber=var_txt_saleOrderERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入销售订单号")

    }else{
      token= dms_token

      FName=var_saleOrderERP2()
      data=mdlCpEcsBillr::saleOrderErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_saleOrderSourceSync_dataView',data = data )



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
#' saleOrderServer()
saleOrderServer <- function(input,output,session,dms_token) {
  #演示功用1
  saleOrderEcsServer(input,output,session,dms_token)
  saleOrderErpServer(input,output,session,dms_token)


}
