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
#' purchaseStorageEcsServer()
purchaseStorageEcsServer <- function(input,output,session,dms_token) {
  var_txt_purchaseStorageSourceSync_manually <- tsui::var_text('txt_purchaseStorageSourceSync_manually')
  var_purchaseStorageERP=tsui::var_ListChoose1('purchaseStorageERP')
  # var_purchaseStorageERP() <- reactive({
  #   switch(input$purchaseStorageERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_purchaseStorageSourceSync_auto=tsui::var_date('date_purchaseStorageSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_purchaseStorageSourceSync_log,{
    FNumber <- var_txt_purchaseStorageSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入采购入库单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::purchaseStorageLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_purchaseStorageSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_purchaseStorageSourceSync_update,{

    FNumber <- var_txt_purchaseStorageSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购入库单号")

    }else{
      token= dms_token

      FName <- var_purchaseStorageERP()
      mdlCpEcsBillr::purchaseStorageStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_purchaseStorageSourceSync_manually,{
    FNumber <- var_txt_purchaseStorageSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购入库单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_purchaseStorageSourceSync_manually()
      FName <- var_purchaseStorageERP()
      mdlCpEcsBillr::purchaseStorageByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$purchaseStorageByNumber_query,{
    FNumber <- var_txt_purchaseStorageSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购入库单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::purchaseStorageByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_purchaseStorageSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_purchaseStorageSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_purchaseStorageSourceSync_auto()
    FName <- var_purchaseStorageERP()
    mdlCpEcsBillr::purchaseStorageByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_purchaseStorageSourceSync_query,{

    token= dms_token
    FStartDate=var_date_purchaseStorageSourceSync_auto()

    data=mdlCpEcsBillr::purchaseStorageByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_purchaseStorageSourceSync_dataView',data = data )



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
#' purchaseStorageErpServer()
purchaseStorageErpServer <- function(input,output,session,dms_token) {
  var_purchaseStorageERP2=tsui::var_ListChoose1('purchaseStorageERP2')

  # var_purchaseStorageERP() <- reactive({
  #   switch(input$purchaseStorageERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_purchaseStorageERP_manually=tsui::var_text('txt_purchaseStorageERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_purchaseStorageERP_manually,{
    FNumber=var_txt_purchaseStorageERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入采购入库单号")

    }else{
      token= dms_token

      FName=var_purchaseStorageERP2()
      data=mdlCpEcsBillr::purchaseStorageErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_purchaseStorageSourceSync_dataView',data = data )



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
#' purchaseStorageServer()
purchaseStorageServer <- function(input,output,session,dms_token) {
  #演示功用1
  purchaseStorageEcsServer(input,output,session,dms_token)
  purchaseStorageErpServer(input,output,session,dms_token)


}
