#include <mpv/client.h>
#include <mpv/render_gl.h>
#include <SDL.h>

static void *get_proc_address_mpv(void *fn_ctx, const char *name) {
  return SDL_GL_GetProcAddress(name);
}

void* create_mpv_gl_ctx(mpv_handle* mpv) {
  int res;
  mpv_render_param params[] = {
    {MPV_RENDER_PARAM_API_TYPE, MPV_RENDER_API_TYPE_OPENGL},
    {MPV_RENDER_PARAM_OPENGL_INIT_PARAMS, &(mpv_opengl_init_params){
        .get_proc_address = get_proc_address_mpv,
      }},
    {0}
  };

  mpv_render_context *mpv_gl;
  res = mpv_render_context_create(&mpv_gl, mpv, params);
  if (res < 0) {
    return 0;
  }
  return mpv_gl;
}

void render_to_default_fbo(mpv_render_context *mpv_gl, int w, int h) {
  mpv_render_param params[] = {
    {MPV_RENDER_PARAM_OPENGL_FBO, &(mpv_opengl_fbo){
        .fbo = 0,
        .w = w,
        .h = h,
      }},
    {MPV_RENDER_PARAM_FLIP_Y, &(int){1}},
    {0}
  };
  mpv_render_context_render(mpv_gl, params);
}
